package io.github.edadma.kutter

import io.github.edadma.suit.*
import io.github.edadma.suit.dsl.*
import io.github.edadma.suit.widgets.*
import io.github.edadma.mlt.Mlt
import io.github.edadma.logger.{LoggerFactory, LogLevel}
import scala.io.Source

// kutter — a fixed-track video editor built on the suit toolkit over the MLT multimedia framework.
// Media is imported into a bin and placed onto tracks (V1/V2 video, composited; A1/A2 audio, mixed),
// which the player compiles into an MLT tractor and decodes on a thread of its own (see [[Player]]),
// showing each frame in a GPU-side video layer that suit blits under the UI chrome. Lower thirds ride
// on top of the whole stack — the automated typeset lower thirds are the reason kutter exists.
//
// The frame path is the one the MLT binding and suit's video layer were built to meet in the
// middle: a bare MLT consumer pulls and decodes off the UI thread, and hands ready YUV planes to
// the UI thread through `UiThread.post`, where they upload into the `VideoTexture` the `video`
// widget paints a hole for.
//
// The pieces that make up the app are split across files in this package: `Icons` (the SVG glyphs),
// `Session`/`SessionStore` (what a run opens with, remembered in the cache), `Diagnostics` (the
// windowless `KUTTER_PROBE*` checks), `Player` (the playback engine), `Timeline` (the track painters),
// `Project` (the data model), and `CardRenderer`/`TexishCard` (the lower-third look). This file holds
// the suit component and the entry point.

// The working timeline length (in frames at the 30fps profile) a project has before any footage is
// placed, so lower thirds can be laid out ahead of the video — 10 seconds.
private val DefaultTimelineFrames = 300

// A pending confirmation: the dialog shows its title and message, and runs `action` on confirm. One
// state drives every destructive prompt (remove a clip, clear the project), rather than a flag each.
private final case class ConfirmSpec(title: String, message: String, confirmLabel: String, action: () => Unit)

/** Format a frame count as m:ss.cc (hundredths of a second) at the given frame rate, for the
  * transport's time readout. */
private def timecode(frames: Int, fps: Double): String =
  val cs   = math.max(0L, (frames / fps * 100).toLong) // centiseconds, floored
  val secs = cs / 100
  f"${secs / 60}:${secs % 60}%02d.${cs % 100}%02d"

private val App: Component[Session] = component[Session] { initial =>
  val theme = Theme.dark

  // The project is editing state — the source of truth the UI mutates and the player renders. It is
  // seeded from the one opened on the command line; every edit replaces it (see `editProject`), which
  // re-renders the panels and pushes the new project to the player to recompile its graph.
  val (project, setProject, updateProject) = useState(initial.project)

  // The `.kutter` file the project is bound to, if any: where Save writes without re-prompting, the
  // default location the save panel opens at, and the name shown in the title bar. `None` until the
  // project is first saved (or if it was opened from a bare media path).
  val (path, setPath, _) = useState(initial.path)

  // Which lower third is selected, by id — what the inspector edits, and the highlighted row in the
  // bin panel. `None` when nothing is selected (the inspector then shows its hint).
  val (selectedId, setSelectedId, _) = useState[Option[String]](None)

  // The pending confirmation, if any — a destructive action (remove a clip, clear the project) held
  // until the user confirms it in the dialog.
  val (confirm, setConfirm, _) = useState[Option[ConfirmSpec]](None)

  // The video layer, once the player has opened the project and created its texture. Null until the
  // mount effect runs, so the first render shows a placeholder rather than an empty hole.
  val (layer, setLayer, _) = useState[VideoLayer | Null](null)
  val (playing, setPlaying, _) = useState(false) // a project loads paused on its first frame, like an editor
  val (volume, setVolume, _)   = useState(initial.project.master)

  // The playback position, polled from the player a few times a second to drive the progress bar.
  // The player exposes it as a plain volatile read (the decode thread writes it), so this never
  // touches the graph.
  val (frame, setFrame, _) = useState(0)

  // The playback engine. Held in a ref because it is imperative state owned across renders, not
  // something the view derives from — the handlers reach it, the view does not.
  val playerRef = useRef[Player | Null](null)

  // The current video texture, kept beside the player so a re-open (opening a project on different
  // media) can destroy the old one and install the new. The unmount cleanup tears down whatever these
  // refs hold, not the pair captured at mount, so a mid-session swap is safe.
  val textureRef = useRef[VideoTexture | Null](null)

  // Scrubbing state. While the pointer is held on the bar, playback is paused and the position
  // follows the cursor; `wasPlaying` remembers whether to resume on release. Refs, not state,
  // because the drag handlers and the poll read them imperatively without needing a re-render.
  val scrubbing  = useRef(false)
  val wasPlaying = useRef(false)

  // Dragging a title block along the timeline. On grab we remember which lower third (`dragId`), the
  // frame the cursor grabbed at (`dragGrab`), and the block's window at that moment (`dragIn` /
  // `dragLen`), so each move places the block relative to the grab with no drift; `dragLastIn` skips a
  // redundant edit when the frame under the cursor hasn't changed. `dragId == null` means no drag.
  val dragId     = useRef[String | Null](null)
  val dragGrab   = useRef(0)
  val dragIn     = useRef(0)
  val dragLen    = useRef(0)
  val dragLastIn = useRef(0)

  // Poll the player's position to drive the scrubber — but not while scrubbing, when the drag owns
  // the position and a poll would fight the cursor.
  useInterval(
    () =>
      if !scrubbing.current then
        playerRef.current match
          case p: Player => setFrame(p.position)
          case null      => (),
    100,
  )

  // A project has something to play — and so wants a player and preview — once it has media on a track
  // or any lower thirds. A titles-only project previews over a black background (see
  // `Player.buildGraph`), so a texish card can be checked before any footage is placed.
  def hasContent(p: Project): Boolean = p.hasMedia || p.lowerThirds.nonEmpty

  // Open a player and its texture for `p` and show it, paused on the first frame. Runs on the UI thread
  // (the texture needs the renderer, and MLT producer creation must stay on the main thread). Used at
  // mount and whenever content first appears — e.g. lower thirds imported before any footage.
  def openPlayerFor(p: Project): Unit =
    val (player, texture) = Player.open(p)
    // When the project plays out and the player stops itself, flip the control back to "Play"; the
    // callback is posted onto the UI thread by the player, so setting state here is safe.
    player.onEnded     = () => setPlaying(false)
    playerRef.current  = player
    textureRef.current = texture
    player.start()
    player.seek(0) // paused on the first frame; the user presses play when ready
    setLayer(texture)

  useEffect(
    () =>
      if hasContent(initial.project) then openPlayerFor(initial.project)
      () =>
        // Tear down whatever the refs currently hold — a re-open may have swapped in a new pair.
        playerRef.current match
          case p: Player => p.close()
          case null      => ()
        textureRef.current match
          case t: VideoTexture => t.destroy()
          case null            => ()
    ,
    Array(),
  )

  // Push each edit to the player. The mount ref skips the first run (on mount the project still equals
  // the one just opened). If there is no player yet but the project has gained content — a lower third
  // imported or added into an empty project — open one now, so the preview appears without a video.
  val edited = useRef(false)
  useEffect(
    () =>
      if edited.current then
        playerRef.current match
          case p: Player => p.update(project)
          case null      => if hasContent(project) then openPlayerFor(project)
      else edited.current = true
      () => ()
    ,
    Array(project),
  )

  // Keep the window title naming the bound file (via suit's WindowControl seam, installed by the
  // runtime), so the title bar reads "kutter — project.kutter" once saved or opened.
  useEffect(
    () =>
      WindowControl.setTitle("kutter" + path.map(p => s" — ${p.split('/').lastOption.getOrElse(p)}").getOrElse(""))
      () => ()
    ,
    Array(path),
  )

  // Remember the working session in the cache on every change, so reopening the app reloads exactly
  // this project and its bound file. The app keeps no hardcoded project — this cache is its memory.
  useEffect(
    () =>
      SessionStore.save(Session(project, path))
      () => ()
    ,
    Array[Any](project, path),
  )

  // Whether the project has edits not yet written to a `.kutter` file. Set on every edit, cleared on
  // save, open, and clearing to a new project — it gates the "discard unsaved changes?" confirmation.
  // A ref, not state: only handlers read it, so it needs no re-render.
  val dirty = useRef(false)

  // Replace the project through `f` — the single funnel every edit goes through, so a change always
  // re-renders the panels, reaches the player (via the effect above), and marks the project unsaved.
  def editProject(f: Project => Project): Unit =
    dirty.current = true
    updateProject(f)

  // Edit the one lower third with `id`, leaving the rest untouched.
  def editLt(id: String, f: LowerThird => LowerThird): Unit =
    editProject(p => p.copy(lowerThirds = p.lowerThirds.map(lt => if lt.id == id then f(lt) else lt)))

  def toggle(): Unit =
    playerRef.current match
      case p: Player =>
        if p.isPlaying then p.pause() else p.play()
        setPlaying(p.isPlaying)
      case null => ()

  // The timeline's length and frame rate. With a project open they are the player's. Before then the
  // project still has a working timeline — long enough to lay out lower thirds ahead of footage, and
  // never shorter than the overlays already reach — so a title can be authored, positioned, and
  // previewed-in-place before a video is placed. The rate is the fixed playback profile's 30fps.
  val (total, fps) = playerRef.current match
    case p: Player => (p.totalFrames, p.fps)
    case null      => (math.max(DefaultTimelineFrames, project.lowerThirds.map(_.outFrame + 1).maxOption.getOrElse(0)), 30.0)
  val progress = math.min(1.0, frame.toDouble / total)

  // The timeline lanes, one per project track, read straight off the player. Empty until it opens.
  // Their generators fill in on their own threads; the position poll re-renders often enough that
  // they appear as they are made.
  val mediaLanes = playerRef.current match
    case p: Player => p.mediaLanes
    case null      => Nil

  // Seek the displayed position to `fraction` of the timeline immediately (so the thumb tracks with no
  // decode round-trip) and ask the player to render that frame.
  def seekToFraction(fraction: Double): Unit =
    val f = math.round(fraction * total).toInt
    setFrame(f)
    playerRef.current match
      case p: Player => p.seek(f)
      case null      => ()

  // Begin a scrub (from the timeline): pause and hold, remembering whether to resume, and jump to
  // `frame`. Shares the drag state with the transport scrubber's brackets.
  def beginScrub(frame: Int): Unit =
    playerRef.current match
      case p: Player =>
        scrubbing.current  = true
        wasPlaying.current = p.isPlaying
        p.pause()
        setFrame(frame)
        p.seek(frame)
      case null => ()

  // Continue a scrub: track the cursor to `frame`.
  def scrubTo(frame: Int): Unit =
    setFrame(frame)
    playerRef.current match
      case p: Player => p.seek(frame)
      case null      => ()

  // End a scrub: resume if playback was running when it began.
  def endScrub(): Unit =
    scrubbing.current = false
    if wasPlaying.current then
      playerRef.current match
        case p: Player => p.play()
        case null      => ()

  // Begin dragging the title block `id`, grabbed at `grabFrame`: select it and snapshot its window so
  // the move is relative to the grab.
  def beginOverlayDrag(id: String, grabFrame: Int): Unit =
    setSelectedId(Some(id))
    project.lowerThirds.find(_.id == id).foreach { lt =>
      dragId.current     = id
      dragGrab.current   = grabFrame
      dragIn.current     = lt.inFrame
      dragLen.current    = lt.outFrame - lt.inFrame
      dragLastIn.current = lt.inFrame
    }

  // Continue a title drag: shift the block by how far the cursor has moved from the grab, keeping its
  // length and clamping the whole block within the timeline. Only edits when the target frame changes,
  // so a press that doesn't move stays a plain selection and drives no graph rebuild.
  def dragOverlay(curFrame: Int): Unit =
    dragId.current match
      case id: String =>
        val newIn = Timeline.dragPlacement(dragIn.current, dragLen.current, dragGrab.current, curFrame, total)
        if newIn != dragLastIn.current then
          dragLastIn.current = newIn
          editLt(id, _.copy(inFrame = newIn, outFrame = newIn + dragLen.current))
      case null => ()

  // End a title drag.
  def endOverlayDrag(): Unit = dragId.current = null

  // The scrubber is suit's Slider: grabbing it pauses and holds on the frame (remembering whether to
  // resume), dragging seeks, and releasing resumes if it was playing. The Slider owns the pointer
  // capture, direct cursor tracking, and the played-progress fill; kutter only supplies the playback
  // behaviour through the drag brackets.
  val scrubber = Slider(
    value    = progress,
    onChange = seekToFraction,
    onChangeStart = _ =>
      playerRef.current match
        case p: Player =>
          scrubbing.current  = true
          wasPlaying.current = p.isPlaying
          p.pause()
        case null => (),
    onChangeEnd = _ =>
      scrubbing.current = false
      if wasPlaying.current then
        playerRef.current match
          case p: Player => p.play()
          case null      => (),
  )

  // A clickable icon: a padded, rounded hit target around a vector glyph.
  def iconButton(icon: SvgImage, onClick: () => Unit): VNode =
    box(
      onClick = _ => onClick(),
      cursor  = Cursor.Pointer,
      padding = EdgeInsets.all(8),
      radius  = 8,
    )(svg(icon, width = 22, height = 22))

  // Master volume: drive both the displayed level and the player's audio gain, and persist it on the
  // project so a reopen restores it (the project's master fader).
  def onVolume(v: Double): Unit =
    setVolume(v)
    playerRef.current match
      case p: Player => p.setVolume(v)
      case null      => ()
    editProject(_.copy(master = v))

  // The transport: the scrubber over a row of play / timecode / frame index / name / master volume.
  val transport =
    box(bg = theme.surface, padding = EdgeInsets.symmetric(horizontal = 16, vertical = 10))(
      col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 10)(
        scrubber,
        row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 12)(
          iconButton(if playing then pauseIcon else playIcon, () => toggle()),
          text(s"${timecode(frame, fps)} / ${timecode(total, fps)}", size = 14, color = theme.surfaceText, mono = true),
          text(s"frame $frame / $total", size = 12, color = theme.border, mono = true),
          spacer(),
          text(project.bin.headOption.map(_.name).getOrElse(project.name), size = 13, color = theme.surfaceText),
          svg(volumeIcon, width = 18, height = 18),
          sizedBox(width = 90)(Slider(volume, onVolume)),
        ),
      ),
    )

  // Wrap content in a rounded, bordered card — the editor's panel shell. It clips, so a video's
  // letterbox or a canvas's fill honours the rounded corners.
  def panel(flexN: Int = 0, h: Double = Double.NaN)(child: VNode): VNode =
    box(bg = theme.surface, flex = flexN, height = h, radius = 10, border = theme.border, borderWidth = 1, clip = true)(
      child,
    )

  // A titled panel: a small header bar over the panel's body — the shell the bin and the inspector use,
  // matching a Resolve-style editor's labelled panes.
  def titledPanel(title: String)(body: VNode): VNode =
    panel(flexN = 1)(
      col(crossAxisAlignment = CrossAxisAlignment.Stretch)(
        box(bg = theme.background, padding = EdgeInsets.symmetric(horizontal = 12, vertical = 8))(
          text(title, size = 12, weight = FontWeight.Bold, color = theme.surfaceText),
        ),
        box(flex = 1, padding = EdgeInsets.all(12))(body),
      ),
    )

  // A placeholder body for a panel whose feature isn't built yet — a centred hint.
  def placeholder(hint: String): VNode =
    col(mainAxisAlignment = MainAxisAlignment.Center, crossAxisAlignment = CrossAxisAlignment.Center)(
      text(hint, size = 13, color = theme.border),
    )

  // Add a lower third and select it for editing. It opens around the current playhead — a sensible
  // default a user then tunes in the inspector — and starts on the first style. A lower third is
  // project data and can be added at any time, footage or not.
  def addLowerThird(): Unit =
    val id    = s"lt-${System.nanoTime()}"
    val len   = math.min(90, math.max(1, total - 1))
    val start = math.max(0, math.min(frame, total - 1 - len))
    val lt    = LowerThird(id, "Name", "Title", start, start + len)
    editProject(p => p.copy(lowerThirds = p.lowerThirds :+ lt))
    setSelectedId(Some(id))

  // Remove a lower third, clearing the selection if it was the one removed.
  def removeLowerThird(id: String): Unit =
    editProject(p => p.copy(lowerThirds = p.lowerThirds.filterNot(_.id == id)))
    if selectedId.contains(id) then setSelectedId(None)

  // Tear down the player and its texture and drop the refs — shared by removing a clip and clearing the
  // project. Leaves the project state to the caller.
  def teardownPlayer(): Unit =
    playerRef.current match
      case cur: Player => cur.close()
      case null        => ()
    textureRef.current match
      case t: VideoTexture => t.destroy()
      case null            => ()
    playerRef.current  = null
    textureRef.current = null
    setLayer(null)
    edited.current = false

  // Clear to a new empty project: empty bin, empty default tracks, no lower thirds, unbound. The full
  // reset behind "New".
  def newProject(): Unit =
    teardownPlayer()
    dirty.current = false
    setProject(Project.blank)
    setSelectedId(None)
    setPath(None)

  // "New" — clear the project, first confirming when there are unsaved changes so work is not lost.
  def requestNew(): Unit =
    if dirty.current then
      setConfirm(Some(ConfirmSpec(
        "Discard unsaved changes?",
        "The project has changes that haven't been saved. Start a new empty project?",
        "Discard",
        () => newProject(),
      )))
    else newProject()

  // The `.kutter` file filter both project panels use.
  val kutterFilter = Seq(FileDialog.Filter("Kutter project", "kutter"))

  // Write the project to `p` (forcing the `.kutter` extension), bind to it, and let the title effect
  // retitle. Runs on the UI thread — the dialog callback is delivered there.
  def saveTo(p: String): Unit =
    val target = if p.endsWith(".kutter") then p else s"$p.kutter"
    project.save(target)
    dirty.current = false
    setPath(Some(target))

  // Save: write straight back to the bound file when there is one, else prompt for a location.
  def doSave(): Unit =
    path match
      case Some(p) => saveTo(p)
      case None =>
        FileDialog.save(filters = kutterFilter, defaultLocation = s"${project.name}.kutter", title = "Save project") {
          case FileDialog.Result.Chosen(paths) => saveTo(paths.head)
          case _                               => ()
        }

  // Re-open from scratch: used when the project's media changes (a clip imported or removed, or a
  // project loaded on different media), so nothing the player built (length, texture, audio, filmstrip,
  // waveform) still fits. Close the current player and texture, open a fresh pair, and reset the edit
  // guard so the freshly opened project is not immediately re-pushed to the player.
  def reopen(p: Project, boundPath: Option[String]): Unit =
    playerRef.current match
      case cur: Player => cur.close()
      case null        => ()
    textureRef.current match
      case t: VideoTexture => t.destroy()
      case null            => ()
    setLayer(null)
    if hasContent(p) then
      val (np, tex) = Player.open(p)
      np.onEnded         = () => setPlaying(false)
      playerRef.current  = np
      textureRef.current = tex
      np.start()
      np.seek(0) // load paused on the first frame rather than auto-playing
      setLayer(tex)
    else
      playerRef.current  = null
      textureRef.current = null
    edited.current = false
    setPlaying(false)
    setProject(p)
    setPath(boundPath)

  // The media-file filters for importing a clip into the bin.
  val videoFilter = Seq(FileDialog.Filter("Video", "mp4;mov;m4v;mkv;webm;avi"))
  val audioFilter = Seq(FileDialog.Filter("Audio", "mp3;wav;m4a;aac;flac;ogg"))

  // Import a clip of `kind` into the bin and append it to the end of the first track of that kind, then
  // re-open the player on it. Placing a clip changes the timeline lanes (each owns a background
  // generator), so this rebuilds the player from scratch rather than the live graph-swap the lower-third
  // edits use. The clip's length is measured against the profile (a producer, so on the UI/main thread).
  // Stage 1 appends; dragging a bin clip to an arbitrary track position comes with the timeline UI.
  def importClip(pathStr: String, kind: MediaKind): Unit =
    dirty.current = true
    val len  = Player.mediaLength(pathStr)
    val clip = MediaClip.make(pathStr, kind)
    val withClip = project.copy(bin = project.bin :+ clip)
    val placed = project.tracks.find(_.kind == kind) match
      case Some(track) =>
        withClip.updateTrack(track.id)(t => t.copy(clips = t.clips :+ PlacedClip.make(clip.id, t.contentEnd, len)))
      case None => withClip
    reopen(placed, path)

  def doImportVideo(): Unit =
    FileDialog.open(filters = videoFilter, title = "Import video") {
      case FileDialog.Result.Chosen(paths) => importClip(paths.head, MediaKind.Video)
      case _                               => ()
    }

  def doImportAudio(): Unit =
    FileDialog.open(filters = audioFilter, title = "Import audio") {
      case FileDialog.Result.Chosen(paths) => importClip(paths.head, MediaKind.Audio)
      case _                               => ()
    }

  // Remove a bin clip and every placement that referenced it, keeping the lower thirds — footage and
  // overlays are independent. If anything remains to preview (another clip, or a lower third over
  // black), re-open on it; otherwise tear the player down to the empty preview.
  def removeBinClip(id: String): Unit =
    dirty.current = true
    val p = project.copy(
      bin = project.bin.filterNot(_.id == id),
      tracks = project.tracks.map(t => t.copy(clips = t.clips.filterNot(_.clipId == id))),
    )
    reopen(p, path)

  // The media on the timeline as a comparable key — each track's kind and its placements (source path,
  // in-point, length, start) in order — so opening a project with the same media reuses the live
  // graph-swap and a different arrangement re-opens.
  def mediaKey(p: Project): Seq[Any] =
    p.tracks.map(t => (t.kind, t.ordered.map(pc => (p.clipFor(pc.clipId).map(_.path), pc.inPoint, pc.length, pc.timelineStart))))

  // Apply a project loaded from `loadedPath`: if it arranges the same media as the current project, the
  // live rebuild handles it (just swap the project state); otherwise everything the player built is tied
  // to the old arrangement, so re-open from scratch.
  def applyOpened(p: Project, loadedPath: String): Unit =
    dirty.current = false // freshly loaded from disk, so it matches its file
    if playerRef.current != null && mediaKey(p) == mediaKey(project) then
      setProject(p)
      setSelectedId(None)
      setPath(Some(loadedPath))
    else reopen(p, Some(loadedPath))

  // Open: prompt for a `.kutter` file, load it, and apply it (live swap or full re-open).
  def doOpen(): Unit =
    FileDialog.open(filters = kutterFilter, title = "Open project") {
      case FileDialog.Result.Chosen(paths) =>
        Project.load(paths.head) match
          case Right(p) => applyOpened(p, paths.head)
          case Left(err) =>
            LoggerFactory.getLogger.error(s"could not open '${paths.head}': $err", category = "player")
      case _ => ()
    }

  // The lower-thirds list filter for Import — kutter's own `.klt` (a HOCON document; see BatchImport).
  val importFilter = Seq(FileDialog.Filter("Lower thirds", "klt"))

  // Import a `.klt` list of lower thirds: prompt for the file, parse it (times to frames at the current
  // rate, a row's style defaulting to the project's first), tag each with the file it came from, and
  // merge — a re-import of the SAME file replaces the lower thirds it produced before (edit the file,
  // re-import, and the old set is swapped for the new) rather than piling duplicates up. The first
  // imported overlay is selected; a parse error names the offending row and nothing changes.
  def doImportLowerThirds(): Unit =
    FileDialog.open(filters = importFilter, title = "Import lower thirds") {
      case FileDialog.Result.Chosen(paths) =>
        val file         = paths.head
        val defaultStyle = project.styles.headOption.map(_.id).getOrElse("broadcast-blue")
        val result =
          try
            val src = Source.fromFile(file)
            try BatchImport.parse(src.mkString, fps, defaultStyle)
            finally src.close()
          catch case e: Exception => Left(if e.getMessage != null then e.getMessage else e.toString)
        result match
          case Right(lts) =>
            val tagged = lts.map(_.copy(source = Some(file)))
            editProject(p => p.copy(lowerThirds = p.lowerThirds.filterNot(_.source.contains(file)) ++ tagged))
            tagged.headOption.foreach(lt => setSelectedId(Some(lt.id)))
          case Left(err) =>
            LoggerFactory.getLogger.error(s"could not import '$file': $err", category = "player")
      case _ => ()
    }

  // A small text button — a rounded, padded hit target used for the panel's actions.
  def textButton(label: String, onClick: () => Unit, enabled: Boolean = true): VNode =
    box(onClick = if enabled then (_ => onClick()) else (_ => ()),
      cursor = if enabled then Cursor.Pointer else Cursor.Default, bg = theme.background, radius = 6,
      padding = EdgeInsets.symmetric(horizontal = 10, vertical = 6))(
      text(label, size = 12, color = if enabled then theme.surfaceText else theme.border),
    )

  // One lower-third row in the bin panel: a selectable body (name over title) and a remove button. The
  // selected row is filled with the primary colour so it reads as the inspector's subject.
  def ltRow(lt: LowerThird): VNode =
    val selected = selectedId.contains(lt.id)
    row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 6)(
      box(onClick = _ => setSelectedId(Some(lt.id)), cursor = Cursor.Pointer, flex = 1, radius = 6,
        bg = if selected then theme.primary else theme.background,
        padding = EdgeInsets.symmetric(horizontal = 8, vertical = 6))(
        col(mainAxisSize = MainAxisSize.Min, spacing = 1)(
          text(if lt.name.isEmpty then "(untitled)" else lt.name, size = 12,
            color = if selected then theme.onPrimary else theme.surfaceText),
          text(lt.title, size = 10, color = if selected then theme.onPrimary else theme.border),
        ),
      ),
      box(onClick = _ => removeLowerThird(lt.id), cursor = Cursor.Pointer, radius = 4, padding = EdgeInsets.all(4))(
        svg(closeIcon, width = 12, height = 12),
      ),
    )

  // One bin row: a kind icon, the file name, and a remove button that confirms first (removing a source
  // takes its placements off the timeline with it). The lower thirds are kept — footage and overlays are
  // independent.
  def binClipRow(clip: MediaClip): VNode =
    row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 8)(
      svg(if clip.kind == MediaKind.Audio then volumeIcon else playIcon, width = 16, height = 16),
      box(flex = 1)(text(clip.name, size = 13, color = theme.surfaceText, maxLines = 1)),
      box(onClick = _ => setConfirm(Some(ConfirmSpec(
        "Remove clip?",
        s"Remove “${clip.name}” from the project? Its placements on the timeline go with it; the lower thirds are kept.",
        "Remove",
        () => removeBinClip(clip.id),
      ))), cursor = Cursor.Pointer, radius = 4, padding = EdgeInsets.all(4))(svg(closeIcon, width = 12, height = 12)),
    )

  // The bin panel (left): the project's imported source clips and its lower thirds, each a titled
  // section with actions on the right (import a video or audio clip / add a lower third). Both can be
  // built up with or without the other — a project is its overlays and its footage, independently — and
  // Open/Save/Import act on the project as a whole. Selecting a lower-third row drives the inspector.
  val binPanel = titledPanel("Bin")(
    col(crossAxisAlignment = CrossAxisAlignment.Stretch, spacing = 10)(
      // Project actions: start over, open/save a `.kutter`.
      row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 6)(
        textButton("New", () => requestNew()),
        textButton("Open", () => doOpen()),
        textButton("Save", () => doSave()),
      ),
      // Media section: the imported clips (video and audio), each removable, with import actions — a
      // project can hold several videos (composited on video tracks) and several audio clips (mixed).
      row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 6)(
        text("Media", size = 11, weight = FontWeight.Bold, color = theme.border),
        spacer(),
        textButton("+ Video", () => doImportVideo()),
        textButton("+ Audio", () => doImportAudio()),
      ),
      if project.bin.isEmpty then placeholder("No media imported")
      else
        col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 4)(
          project.bin.map(binClipRow)*,
        ),
      // Lower thirds section: import a `.klt` list or add one by hand — both work with or without footage.
      row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 6)(
        text("Lower thirds", size = 11, weight = FontWeight.Bold, color = theme.border),
        spacer(),
        textButton("Import…", () => doImportLowerThirds()),
        textButton("+ Add", () => addLowerThird()),
      ),
      box(flex = 1)(
        if project.lowerThirds.isEmpty then placeholder("No lower thirds yet")
        else
          scrollView(axis = Axis.Vertical, scrollbar = true, scrollbarThumb = theme.border)(
            col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 3)(
              project.lowerThirds.map(ltRow)*,
            ),
          ),
      ),
    ),
  )

  // The video preview — the top of the player panel. With a project it shows the picture; with one
  // opening, a brief notice. With nothing it is just black: the bin is where media is imported, so the
  // preview needs no prompt of its own.
  val preview =
    box(bg = Color.black, flex = 1)(
      layer match
        case l: VideoLayer => video(l, fit = VideoFit.Contain)
        case null if hasContent(project) =>
          box(flex = 1)(
            col(mainAxisAlignment = MainAxisAlignment.Center, crossAxisAlignment = CrossAxisAlignment.Center)(
              text("Opening…", size = 16, color = theme.surfaceText),
            ),
          )
        case null => box(flex = 1)(),
    )

  // The player panel (centre): the preview and its transport, grouped as one card.
  val playerPanel =
    panel(flexN = 1)(
      col(crossAxisAlignment = CrossAxisAlignment.Stretch)(preview, transport),
    )

  // A labelled inspector control: a small caption over the field.
  def labeled(label: String)(control: VNode): VNode =
    col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 4)(
      text(label, size = 11, weight = FontWeight.Bold, color = theme.border),
      control,
    )

  // A whole-number field: a text field that commits only a value that parses, so a partial or empty
  // entry leaves the model untouched rather than snapping it to zero.
  def intField(value: Int, onChange: Int => Unit): VNode =
    TextField(value.toString, s => s.trim.toIntOption.foreach(onChange))

  // The inspector body for the selected lower third: its words, its style, and its timing. Every edit
  // funnels through `editLt`, so a keystroke re-renders and the player recompiles its graph.
  def inspectorBody(lt: LowerThird): VNode =
    col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 12)(
      labeled("Name")(TextField(lt.name, v => editLt(lt.id, _.copy(name = v)))),
      labeled("Title")(TextField(lt.title, v => editLt(lt.id, _.copy(title = v)))),
      labeled("Style")(
        Select(
          options  = project.styles.map(s => (s.id, s.label)),
          selected = lt.styleId,
          onChange = v => editLt(lt.id, _.copy(styleId = v)),
          width    = 200,
        ),
      ),
      row(crossAxisAlignment = CrossAxisAlignment.Start, spacing = 8)(
        box(flex = 1)(labeled("In")(intField(lt.inFrame, v => editLt(lt.id, _.copy(inFrame = v))))),
        box(flex = 1)(labeled("Out")(intField(lt.outFrame, v => editLt(lt.id, _.copy(outFrame = v))))),
        box(flex = 1)(labeled("Fade")(intField(lt.fadeFrames, v => editLt(lt.id, _.copy(fadeFrames = v))))),
      ),
    )

  // The inspector (right): the selected lower third's editor, or a hint when nothing is selected.
  val selectedLt     = selectedId.flatMap(id => project.lowerThirds.find(_.id == id))
  val inspectorPanel = titledPanel("Inspector")(
    selectedLt match
      case Some(lt) => inspectorBody(lt)
      case None     => placeholder("Select a lower third"),
  )

  // The lane colour for a lower third, drawn from its style so a block on the timeline reads as the
  // look it wears: the accent stripe if the style has one, else the bar if it is solid enough, else
  // the theme accent (for a bar-less style like "minimal", whose card is all text).
  def blockColor(styleId: String): Color =
    val st  = project.styleFor(styleId)
    val src = st.stripe.getOrElse(st.bar)
    if src.a < 0.35 then theme.accent
    else Color((src.r * 255).round.toInt, (src.g * 255).round.toInt, (src.b * 255).round.toInt)

  // The lower thirds as blocks on the titles lane, each carrying its window, caption, style colour,
  // and whether it is the selected one (so it draws ringed and stays in step with the inspector).
  val overlayBlocks = project.lowerThirds.map(lt =>
    Timeline.OverlayBlock(
      id       = lt.id,
      inFrame  = lt.inFrame,
      outFrame = lt.outFrame,
      label    = if lt.name.nonEmpty then lt.name else lt.title,
      color    = blockColor(lt.styleId),
      selected = selectedId.contains(lt.id),
    ),
  )

  // The tracks, drawn top-to-bottom: one lane per project track (a video track's filmstrip, an audio
  // track's waveform), then the titles lane the lower thirds ride on. When no player is open the empty
  // project tracks (V1, A1) still show as lanes waiting for clips, as in any editor. The titles lane is
  // always present so it is a stable target as overlays are added.
  val mediaTracks =
    if mediaLanes.nonEmpty then
      mediaLanes.map { lane =>
        lane.kind match
          case MediaKind.Video => Timeline.Track(lane.name, lane.length, thumbs = lane.thumbs)
          case MediaKind.Audio => Timeline.Track(lane.name, lane.length, waveform = lane.waveform)
      }
    else project.tracks.map(t => Timeline.Track(t.name, 0))
  val tracks = mediaTracks :+ Timeline.Track("Titles", 0, overlays = overlayBlocks)

  // Scrubbing works from the ruler or any track: mousedown begins, drag seeks (the canvas captures
  // the pointer so it tracks past the widget's edges), release ends. All widgets share the timeline
  // width, so the same frame-under-cursor mapping serves every one.
  def scrubCanvas(paint: (Canvas, Size) => Unit): VNode =
    canvas(
      onMouseDown = e => beginScrub(Timeline.frameAt(e.localX, total, e.size.width)),
      onMouseMove = e => if scrubbing.current then scrubTo(Timeline.frameAt(e.localX, total, e.size.width)),
      onMouseUp = _ => endScrub(),
    )(paint)

  // The time ruler, pinned across the top of the track panel. Its left is a blank column the width of
  // the track labels, so the ruler's time area starts where the track lanes do and the playhead lines
  // up across all of them.
  val ruler =
    box(height = Timeline.RulerHeight)(
      row(crossAxisAlignment = CrossAxisAlignment.Stretch)(
        sizedBox(width = Timeline.LabelWidth)(box(bg = theme.surface)()),
        box(flex = 1)(scrubCanvas((cv, size) => Timeline.paintRuler(cv, size, total, frame, theme))),
      ),
    )

  // A track's name column down the left, as in any editor — a fixed-width label beside the lane.
  def trackLabel(name: String): VNode =
    sizedBox(width = Timeline.LabelWidth)(
      box(bg = theme.surface, padding = EdgeInsets.symmetric(horizontal = 8, vertical = 6))(
        col(mainAxisAlignment = MainAxisAlignment.Center, crossAxisAlignment = CrossAxisAlignment.Start)(
          text(name, size = 11, weight = FontWeight.Bold, color = theme.border),
        ),
      ),
    )

  // One track widget: a left name column beside its lane canvas, which paints that track's clips and
  // its segment of the playhead. On the titles lane a press on a block selects that lower third and
  // begins a drag that slides its window (the inspector's In/Out follow live); a press on the empty
  // part of any lane scrubs.
  def trackWidget(t: Timeline.Track): VNode =
    val paint = (cv: Canvas, size: Size) => Timeline.paintTrack(cv, size, t, total, frame, theme)
    val lane =
      t.overlays match
        case null =>
          scrubCanvas(paint)
        case blocks =>
          canvas(
            onMouseDown = e =>
              Timeline.overlayAt(e.localX, total, e.size.width, blocks) match
                case Some(id) => beginOverlayDrag(id, Timeline.frameAt(e.localX, total, e.size.width))
                case None     => beginScrub(Timeline.frameAt(e.localX, total, e.size.width)),
            onMouseMove = e =>
              val f = Timeline.frameAt(e.localX, total, e.size.width)
              if dragId.current != null then dragOverlay(f)
              else if scrubbing.current then scrubTo(f),
            onMouseUp = _ =>
              endOverlayDrag()
              endScrub(),
          )(paint)
    box(height = Timeline.TrackHeight)(
      row(crossAxisAlignment = CrossAxisAlignment.Stretch)(
        trackLabel(t.name),
        box(flex = 1)(lane),
      ),
    )

  // The track panel (bottom): a card with the pinned ruler over a vertical scroll view of the track
  // widgets, so a tall stack scrolls while the ruler stays put. Each track is its own widget inside
  // the panel rather than one canvas painting them all.
  val trackPanel =
    panel(flexN = 1)(
      col(crossAxisAlignment = CrossAxisAlignment.Stretch)(
        ruler,
        box(flex = 1)(
          scrollView(axis = Axis.Vertical, scrollbar = true, scrollbarThumb = theme.border)(
            col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min)(
              tracks.map(trackWidget)*,
            ),
          ),
        ),
      ),
    )

  // The editor body, Resolve-style: a top row of bin | player | inspector — split by draggable
  // gutters (a horizontal splitter, nested so the player sits between the bin and the inspector) —
  // over the track panel, the two divided by a draggable vertical splitter so the timeline area can
  // be sized. It starts compact.
  val topRow =
    splitter(axis = Axis.Horizontal, initial = 0.18, min = 0.12, max = 0.4)(
      binPanel,
      splitter(axis = Axis.Horizontal, initial = 0.74, min = 0.5, max = 0.86)(
        playerPanel,
        inspectorPanel,
      ),
    )

  // The confirmation modal (from suit), shown while a `ConfirmSpec` is pending. Cancel or the mask
  // dismisses it; the action button runs the pending action. The spec's fields fill the title, message,
  // and action-button label, so one dialog serves every destructive prompt.
  val (cTitle, cMsg, cLabel, cAction) = confirm match
    case Some(s) => (s.title, s.message, s.confirmLabel, s.action)
    case None    => ("", "", "OK", () => ())
  val confirmDialog =
    Dialog(open = confirm.isDefined, onClose = () => setConfirm(None), width = 380)(
      // A fixed inner width so the message wraps (the dialog does not constrain its content on its own).
      sizedBox(width = 336)(
        col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 16)(
          text(cTitle, size = 16, weight = FontWeight.Bold, color = theme.surfaceText),
          text(cMsg, size = 13, color = theme.border, maxLines = 0),
          row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 10)(
            spacer(),
            textButton("Cancel", () => setConfirm(None)),
            textButton(cLabel, () => { setConfirm(None); cAction() }),
          ),
        ),
      ),
    )

  ThemeProvider(theme)(
    box(bg = theme.background, padding = EdgeInsets.all(8))(
      splitter(axis = Axis.Vertical, initial = 0.6, min = 0.35, max = 0.9)(
        topRow,
        trackPanel,
      ),
      confirmDialog,
    ),
  )
}

@main def main(args: String*): Unit =
  val arg = args.headOption.getOrElse("big_buck_bunny_720p.mp4")

  // Quiet by default; KUTTER_DEBUG turns on the frame-path trace (opened graph, first frames,
  // uploads). One env var, no rebuild.
  if sys.env.contains("KUTTER_DEBUG") then LoggerFactory.getLogger.setLogLevel(LogLevel.TRACE)

  // The windowless probe diagnostics run against a demo project (or a named `.kutter`) so they have
  // overlays to exercise; the real app, further down, starts from a blank or remembered session instead
  // — there is no hardcoded project in normal use. If a probe ran, exit without opening the GUI.
  val project =
    if arg.endsWith(".kutter") then
      Project.load(arg) match
        case Right(p)  => p
        case Left(err) => println(s"kutter: could not read project '$arg': $err"); return
    else Diagnostics.demoProject(arg)

  if Diagnostics.run(project) then return

  Mlt.init()

  // A branded splash in its own small window before the player window opens.
  Splash.show("assets/logo.png", 1000)

  // What the app opens with — no hardcoded project, and (like a real editor) no clip loaded by default.
  // A `.kutter` argument is opened and stays bound to its file. A bare media path resumes the remembered
  // session when it holds that same clip (so reopening reloads the work done last time), otherwise it
  // starts a blank project with that clip placed on V1. With no argument, it resumes whatever session
  // was remembered — the project last worked on — or, when there is none, opens empty (no media),
  // leaving the user to import a video or open a project. The session is re-cached on every edit (see
  // the effect in `App`), which is what makes a reopen reload the contents. The clip's length is
  // measured against the profile, which needs MLT initialised (done above).
  val session =
    args.headOption match
      case Some(a) if a.endsWith(".kutter") => Session(project, Some(a))
      case Some(mediaArg) =>
        SessionStore.load()
          .filter(_.project.bin.exists(_.path == mediaArg))
          .getOrElse(Session(Diagnostics.videoProject(mediaArg, Player.mediaLength(mediaArg)), None))
      case None =>
        SessionStore.load().getOrElse(Session(Project.blank, None))

  Suit.run("kutter", 1100, 720, maximized = true)(App(session))
  Mlt.close()
