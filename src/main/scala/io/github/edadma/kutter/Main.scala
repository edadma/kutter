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

// Which monitor the centre panel shows, and which player the transport drives. The project monitor plays
// the assembled timeline; the clip monitor previews a single bin clip in isolation (as in kdenlive). Only
// one is engaged at a time — switching to the clip monitor pauses the project player and opens a light
// one-clip player; switching back closes it — so at most one player voices audio.
private enum MonitorMode:
  case Project, Clip

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

  // Which placed clip is selected on the timeline, by its placement id — ringed on its lane and shown
  // in the inspector. A clip and a lower third are never selected at once; picking one clears the other.
  val (selectedClipId, setSelectedClipId, _) = useState[Option[String]](None)

  // Which monitor the centre panel shows, and which bin clip (if any) the clip monitor previews. The
  // clip monitor is engaged by picking a bin clip; the Project tab returns to the timeline.
  val (monitorMode, setMonitorMode, _) = useState[MonitorMode](MonitorMode.Project)
  val (selectedBinId, setSelectedBinId, _) = useState[Option[String]](None)

  // The pending confirmation, if any — a destructive action (remove a clip, clear the project) held
  // until the user confirms it in the dialog.
  val (confirm, setConfirm, _) = useState[Option[ConfirmSpec]](None)

  // The project-settings dialog: whether it is open, and the staged values it edits — name, creation
  // date, and the timeline format (resolution, frame rate, audio rate). They are drafts so the dialog
  // can be filled and cancelled without touching the project, and a spec change (which re-opens the
  // player against a new profile) happens once on Apply rather than on every keystroke. Seeded from the
  // project when the dialog opens.
  val (settingsOpen, setSettingsOpen, _) = useState(false)
  val (settingsDraft, setSettingsDraft, updateSettingsDraft) =
    useState(SettingsDraft("", "", TimelineSpec.default))

  // A running video export: whether one is in progress, its 0..1 progress for the bar, and the job it
  // polls plus the path it is writing (refs — the poll reads them imperatively). A poll advances the
  // progress and finishes the job when the encode completes; see the export effect below.
  val (exporting, setExporting, _)             = useState(false)
  val (exportProgress, setExportProgress, _)   = useState(0.0)
  val exportJob = useRef[Player.RenderJob | Null](null)
  val exportOut = useRef("")

  // The video layer, once the player has opened the project and created its texture. Null until the
  // mount effect runs, so the first render shows a placeholder rather than an empty hole.
  val (layer, setLayer, _) = useState[VideoLayer | Null](null)
  val (playing, setPlaying, _) = useState(false) // a project loads paused on its first frame, like an editor
  val (volume, setVolume, _)   = useState(initial.project.master)

  // The project timeline's playhead frame. A ref, not state: the timeline lanes read it while being
  // repainted (not re-rendered), so the playhead can advance during playback without reconciling the
  // whole editor. The poll below advances it and asks for a repaint; handlers that act at the playhead
  // (scrub, place a clip, add a lower third) read and write it directly. The player writes its own
  // `position` from the decode thread; this ref mirrors it for the view.
  val playheadRef = useRef(0)

  // The playback engine. Held in a ref because it is imperative state owned across renders, not
  // something the view derives from — the handlers reach it, the view does not.
  val playerRef = useRef[Player | Null](null)

  // The current video texture, kept beside the player so a re-open (opening a project on different
  // media) can destroy the old one and install the new. The unmount cleanup tears down whatever these
  // refs hold, not the pair captured at mount, so a mid-session swap is safe.
  val textureRef = useRef[VideoTexture | Null](null)

  // The clip monitor's player and texture — a light, one-clip player opened when a bin clip is picked for
  // preview, and closed when the project monitor is shown again, so only one player is ever engaged.
  // `clipPlayerId` records which bin clip it currently shows, so re-selecting the same clip doesn't
  // needlessly reopen it. `clipLayer` is the clip preview's video layer and `clipTotal` its length; the
  // transport polls the clip player's position itself, so there is no `clipFrame` state.
  val clipPlayerRef  = useRef[Player | Null](null)
  val clipTextureRef = useRef[VideoTexture | Null](null)
  val clipPlayerId   = useRef[String | Null](null)
  val (clipLayer, setClipLayer, _) = useState[VideoLayer | Null](null)
  val (clipTotal, setClipTotal, _) = useState(0)

  // Whether the clip monitor is showing, and the player the transport currently drives. Everything the
  // transport does (play/pause, scrub, seek, position) goes through this indirection so one transport
  // serves both monitors; the timeline always drives the project player.
  val isClip = monitorMode == MonitorMode.Clip
  def activePlayer(): Player | Null = if isClip then clipPlayerRef.current else playerRef.current

  // Transport scrub state: while the transport's Slider owns the drag, playback is paused and
  // `wasPlaying` remembers whether to resume on release. A ref because the scrub brackets read it
  // imperatively without a re-render. (The timeline lanes have their own scrub state — see
  // [[TimelinePanel]], which also owns the viewport and every in-flight clip/overlay drag.)
  val wasPlaying = useRef(false)

  // Bumped to refit the timeline view to the whole project — on New, on Open, and when a settings
  // change re-opens the player. [[TimelinePanel]] watches this token and drops its zoom when it
  // changes, so a fresh or reframed project opens from the whole-timeline view rather than inheriting
  // the last one's framing.
  val (resetToken, _, bumpResetToken) = useState(0)
  def resetView(): Unit = bumpResetToken(_ + 1)

  // Advance a running export. The encode runs on MLT's own threads; this poll reads how far it has got
  // for the progress bar, and when it finishes frees the job and shows a completion notice. Reads only
  // refs and stable setters, so the constant-deps interval closure stays correct. A cheap no-op when no
  // export is running.
  useInterval(
    () =>
      exportJob.current match
        case job: Player.RenderJob =>
          if job.isDone then
            job.finish()
            exportJob.current = null
            setExporting(false)
            setExportProgress(1.0)
            setConfirm(Some(ConfirmSpec("Export complete", s"Wrote ${exportOut.current}.", "OK", () => ())))
          else setExportProgress(job.position.toDouble / math.max(1, job.totalFrames))
        case null => (),
    200,
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

  // A one-clip project for the clip monitor: a video clip previews as its linked A/V pair (picture over
  // its own sound), an audio clip on an audio track alone (the preview is black, the sound plays).
  def clipMonitorProject(clip: MediaClip): Project =
    val len = math.max(1, if clip.frames > 0 then clip.frames else Player.mediaLength(clip.path, project.spec))
    val base = clip.kind match
      case MediaKind.Video => Diagnostics.videoProject(clip.path, len)
      case MediaKind.Audio =>
        Project.blank.copy(
          bin = List(clip),
          tracks = List(Track("a1", "A1", MediaKind.Audio, List(PlacedClip.make(clip.id, 0, len)))),
        )
    base.copy(spec = project.spec) // preview the clip conformed to the timeline it will sit on

  // Tear down the clip monitor's player and texture and drop its refs. Safe to call when none is open.
  def closeClipPlayer(): Unit =
    clipPlayerRef.current match
      case p: Player => p.close()
      case null      => ()
    clipTextureRef.current match
      case t: VideoTexture => t.destroy()
      case null            => ()
    clipPlayerRef.current  = null
    clipTextureRef.current = null
    clipPlayerId.current   = null
    setClipLayer(null)

  // Open the clip monitor on `clip`, paused on its first frame, at the current master volume. Replaces
  // any clip player already open. Runs on the UI thread (MLT producer creation must stay on the main
  // thread), so it is only ever called from a handler.
  def openClipPlayer(clip: MediaClip): Unit =
    closeClipPlayer()
    val (player, texture) = Player.open(clipMonitorProject(clip))
    player.onEnded        = () => setPlaying(false)
    clipPlayerRef.current  = player
    clipTextureRef.current = texture
    clipPlayerId.current   = clip.id
    player.start()
    player.setVolume(volume)
    player.seek(0)
    setClipLayer(texture)
    setClipTotal(player.totalFrames)

  // Show the clip monitor for a bin clip: select it, pause the project player, open the clip player (only
  // reopening when a different clip is picked), and switch the centre panel to the clip tab.
  def showClipMonitor(clip: MediaClip): Unit =
    setSelectedBinId(Some(clip.id))
    playerRef.current match
      case p: Player => p.pause()
      case null      => ()
    if clipPlayerRef.current == null || clipPlayerId.current != clip.id then openClipPlayer(clip)
    setMonitorMode(MonitorMode.Clip)
    setPlaying(false)

  // Return to the project monitor: close the clip player (so only the project player remains and voices
  // audio), switch the centre panel back, restore the master volume display, and reflect the project
  // player's play state (it is paused, having been paused when the clip monitor took over).
  def showProjectMonitor(): Unit =
    closeClipPlayer()
    setMonitorMode(MonitorMode.Project)
    setVolume(project.master)
    playerRef.current match
      case p: Player => setPlaying(p.isPlaying)
      case null      => setPlaying(false)

  // Pull focus to the project monitor when the timeline is touched while the clip monitor is showing — a
  // timeline gesture is inherently a project action, so it takes the centre panel back (kdenlive-style).
  def focusProjectMonitor(): Unit =
    if isClip then showProjectMonitor()

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
        closeClipPlayer()
    ,
    Array(),
  )

  // Push each edit to the player. The mount ref skips the first run (on mount the project still equals
  // the one just opened). An edit that leaves the graph's structure alone — only a track's gain or the
  // master level changed — is applied **live** (the master to the audio device, a track's gain to its
  // volume filter) so riding a fader never interrupts playback; anything that changes the media or the
  // lower thirds rebuilds the graph. If there is no player yet but the project has gained content — a
  // lower third imported or added into an empty project — open one now, so the preview appears.
  // The project's graph-shaping signature: the media arrangement (each track's kind and its placements'
  // source/in/length/start) and the lower thirds — but NOT the gains. Two projects with the same
  // signature compile to the same graph, so a change that only moved a fader can be told apart from one
  // that needs a rebuild.
  def graphSig(p: Project): Any =
    (p.tracks.map(t => (t.kind, t.ordered.map(pc => (p.clipFor(pc.clipId).map(_.path), pc.inPoint, pc.length, pc.timelineStart, pc.mc, pc.angle)))),
     p.lowerThirds, p.multicams)

  val edited     = useRef(false)
  val lastPushed = useRef(initial.project)
  useEffect(
    () =>
      if edited.current then
        playerRef.current match
          case p: Player =>
            val prev = lastPushed.current
            if graphSig(project) == graphSig(prev) then
              if project.master != prev.master then p.setVolume(project.master)
              for t <- project.audioTracks do
                if !prev.tracks.find(_.id == t.id).exists(_.gain == t.gain) then p.setTrackGain(t.id, t.gain)
            else p.update(project)
          case null => if hasContent(project) then openPlayerFor(project)
        lastPushed.current = project
      else
        edited.current     = true
        lastPushed.current = project
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

  // Open the project-settings dialog, seeding its draft from the current project.
  def openSettings(): Unit =
    setSettingsDraft(SettingsDraft(project.name, project.created, project.spec))
    setSettingsOpen(true)

  def toggle(): Unit =
    activePlayer() match
      case p: Player =>
        if p.isPlaying then p.pause() else p.play()
        setPlaying(p.isPlaying)
      case null => ()

  // The frame the project's content reaches — the furthest clip or lower third — which the timeline
  // must be at least as long as so a clip dragged toward the end still has room and the ruler covers it.
  val projectExtent = math.max(project.contentEnd, project.lowerThirds.map(_.outFrame + 1).maxOption.getOrElse(0))

  // How far the project's content reaches, and the frame rate. With a project open this is the player's
  // graph length, grown to cover any edit made since it opened (a clip moved past the old end) so the
  // ruler and drag bounds keep up before the next re-open refreshes it. Before a player opens, the
  // project still has a working timeline — long enough to lay out lower thirds ahead of footage. The
  // rate is the profile's.
  val (contentReach, fps) = playerRef.current match
    case p: Player => (math.max(p.totalFrames, projectExtent), p.fps)
    case null      => (math.max(Timeline.DefaultTimelineFrames, projectExtent), project.spec.fps)

  // The timeline's length: the content plus a tail of empty space, so a clip can always be slid right
  // (opening a gap before it for an intro) and a drop near the end has room. The tail grows with the
  // content — projectExtent tracks a moved clip — so within a project the runway never runs out; the
  // graph itself is only sized to the content (the black base ends at `contentReach`), so the tail is
  // purely timeline headroom, not rendered frames.
  val total    = contentReach + math.max(Timeline.TimelineTailFrames, contentReach)

  // The span the "fit" framing shows: the content plus a little margin — not the pan runway, which
  // exists to give placements room, not to be looked at. An empty project fits its default window.
  // Passed to [[TimelinePanel]], which owns the viewport.
  val fitFrames = math.max(Timeline.DefaultTimelineFrames, math.round(projectExtent * 1.05).toInt)

  // The active monitor's length: the *content* reach in the project monitor (what actually plays —
  // the graph ends at the last clip/lower third, not in the empty runway the timeline pads on for
  // dragging), the previewed clip's in the clip monitor. The transport derives its played fraction and
  // its total-time readout from this, so a 20-second project reads 0:20, not the padded timeline length.
  val activeTotal = if isClip then clipTotal else contentReach

  // Seek the active monitor to `fraction` of its length and render that frame. In the project monitor
  // the timeline playhead follows at once (set the ref and repaint); the transport's own readout tracks
  // the cursor through the Slider. In the clip monitor only the clip player seeks.
  def seekToFraction(fraction: Double): Unit =
    val f = math.round(fraction * activeTotal).toInt
    if !isClip then
      playheadRef.current = f
      Repaint.request()
    activePlayer() match
      case p: Player => p.seek(f)
      case null      => ()

  // Unlink the clip `id` from its A/V partner: clear the shared link id on both halves so the picture
  // and its sound move (and later trim) independently. The graph is unchanged — the link is a timeline
  // grouping, not a render property — so this rides the live graph-swap like any other project edit.
  def unlinkPlacement(id: String): Unit =
    val group = project.moveGroupOf(id).map(_._2.id).toSet
    editProject(p => p.copy(tracks = p.tracks.map(t =>
      t.copy(clips = t.clips.map(c => if group.contains(c.id) then c.copy(link = None) else c)))))

  // Set an audio track's fader to the linear gain `v` (0 silent, 1 unity). A volume change leaves the
  // tracks' clips untouched — only the per-track `volume` filter's gain differs — so it rides the live
  // graph-swap (`editProject`→effect→`p.update`), like a lower-third edit, with no re-open.
  def setTrackVolume(id: String, v: Double): Unit =
    editProject(_.updateTrack(id)(_.copy(volume = v)))

  // Toggle an audio track's mute. `Track.gain` is 0 while muted (the fader value is remembered and
  // returns on unmute), and buildGraph reads `gain`, so this too rides the live graph-swap.
  def toggleMute(id: String): Unit =
    editProject(_.updateTrack(id)(t => t.copy(muted = !t.muted)))

  // Begin a scrub from the transport's own Slider: pause the active monitor and remember whether to
  // resume. (The Slider owns the cursor tracking and the played-progress fill; kutter supplies only the
  // playback behaviour through these brackets, which the transport calls.)
  def onScrubStart(): Unit =
    activePlayer() match
      case p: Player =>
        wasPlaying.current = p.isPlaying
        p.pause()
      case null => ()

  // End a transport scrub: resume if it was playing.
  def onScrubEnd(): Unit =
    if wasPlaying.current then
      activePlayer() match
        case p: Player => p.play()
        case null      => ()

  // Master volume: drive the displayed level and the active monitor's audio gain. In the project monitor
  // it is the project master and persists (so a reopen restores it); in the clip monitor it only rides the
  // preview's gain, leaving the project master untouched.
  def onVolume(v: Double): Unit =
    setVolume(v)
    activePlayer() match
      case p: Player => p.setVolume(v)
      case null      => ()
    if !isClip then editProject(_.copy(master = v))

  // What the transport names on its right: the previewed clip in the clip monitor, else the project's
  // first clip (or its name) in the project monitor.
  val transportLabel =
    if isClip then selectedBinId.flatMap(id => project.bin.find(_.id == id)).map(_.name).getOrElse("Clip")
    else project.bin.headOption.map(_.name).getOrElse(project.name)

  // The transport, as its own component so its position readout ticking during playback re-renders only
  // it, not the editor (whose timeline lanes follow the playhead through a repaint instead — see the
  // poll above). It polls the active monitor itself; the editor hands it the values that change rarely
  // (play state, length, name, volume) and the callbacks that reach playback.
  val transport =
    Transport(TransportProps(
      player       = () => activePlayer(),
      total        = activeTotal,
      fps          = fps,
      playing      = playing,
      label        = transportLabel,
      volume       = volume,
      onToggle     = () => toggle(),
      onSeek       = seekToFraction,
      onScrubStart = () => onScrubStart(),
      onScrubEnd   = () => onScrubEnd(),
      onVolume     = onVolume,
    ))

  // Wrap content in a rounded, bordered card — the editor's panel shell. It clips, so a video's
  // letterbox or a canvas's fill honours the rounded corners.
  def panel(flexN: Int = 0, h: Double = Double.NaN)(child: VNode): VNode =
    KutterUi.panel(theme)(flexN, h)(child)

  // Add a lower third and select it for editing. It opens around the current playhead — a sensible
  // default a user then tunes in the inspector — and starts on the first style. A lower third is
  // project data and can be added at any time, footage or not.
  def addLowerThird(): Unit =
    val id    = s"lt-${System.nanoTime()}"
    val len   = math.min(90, math.max(1, total - 1))
    val start = math.max(0, math.min(playheadRef.current, total - 1 - len))
    val lt    = LowerThird(id, "Name", "Title", start, start + len)
    editProject(p => p.copy(lowerThirds = p.lowerThirds :+ lt))
    setSelectedId(Some(id))
    setSelectedClipId(None)

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
    closeClipPlayer()
    setMonitorMode(MonitorMode.Project)
    setSelectedBinId(None)

  // Clear to a new empty project: empty bin, empty default tracks, no lower thirds, unbound. The full
  // reset behind "New".
  def newProject(): Unit =
    teardownPlayer()
    dirty.current = false
    setProject(Project.blank)
    setSelectedId(None)
    setPath(None)
    resetView()

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

  // Apply the settings drafts. A change to the timeline format (resolution, frame rate, or audio rate)
  // re-opens the player against the new profile — the texture, audio device, and graph are all tied to
  // it — and refits the view; a name/date-only change rides the ordinary edit path. The frame rate can
  // only differ here when the timeline had no content (the dialog locks it otherwise), so no placed clip
  // needs its frame counts remapped.
  def applySettings(): Unit =
    setSettingsOpen(false)
    val newSpec = settingsDraft.spec
    val newName = settingsDraft.name.trim match { case "" => "Untitled"; case s => s }
    val renamed = project.copy(name = newName, created = settingsDraft.created.trim)
    if newSpec != project.spec then
      dirty.current = true
      resetView()
      reopen(renamed.withSpec(newSpec), path)
    else if renamed.name != project.name || renamed.created != project.created then
      editProject(_ => renamed)

  // Export the timeline to a video file. Nothing to render on an empty timeline, so that is a gentle
  // notice; otherwise a save dialog picks the path and the encode begins.
  val exportFilter = Seq(FileDialog.Filter("Video", "mp4;mov"))

  def requestExport(): Unit =
    if !hasContent(project) then
      setConfirm(Some(ConfirmSpec("Nothing to export",
        "Place some media or a lower third on the timeline before exporting a video.", "OK", () => ())))
    else
      FileDialog.save(filters = exportFilter, defaultLocation = s"${project.name}.mp4", title = "Export video") {
        case FileDialog.Result.Chosen(paths) => startExport(paths.head)
        case _                               => ()
      }

  // Begin encoding `outPath` (forcing a video extension). Playback is paused first so the export owns
  // the machine, then the render job is started and the progress dialog opens; the poll above advances
  // and finishes it. Producer creation is on this (UI/main) thread, as MLT requires.
  def startExport(outPath: String): Unit =
    val out = if outPath.endsWith(".mp4") || outPath.endsWith(".mov") then outPath else s"$outPath.mp4"
    playerRef.current match
      case p: Player => p.pause(); setPlaying(false)
      case null      => ()
    try
      exportOut.current = out
      exportJob.current = Player.startRender(project, out)
      setExportProgress(0.0)
      setExporting(true)
    catch
      case e: Throwable =>
        setConfirm(Some(ConfirmSpec("Export failed", Option(e.getMessage).getOrElse(e.toString), "OK", () => ())))

  // Cancel a running export: stop and free the job (leaving whatever was written so far on disk).
  def cancelExport(): Unit =
    exportJob.current match
      case job: Player.RenderJob => job.finish()
      case null                  => ()
    exportJob.current = null
    setExporting(false)

  // The media-file filters for importing a clip into the bin.
  val videoFilter = Seq(FileDialog.Filter("Video", "mp4;mov;m4v;mkv;webm;avi"))
  val audioFilter = Seq(FileDialog.Filter("Audio", "mp3;wav;m4a;aac;flac;ogg"))

  // Import a clip of `kind` into the BIN. Importing is not placing: the clip joins the bin's source list
  // and nothing lands on a track — the user places it deliberately (the bin row's Place), choosing where
  // it goes, rather than every import piling onto V1/A1. The clip's length is measured against the profile
  // (a producer, so on the UI/main thread) and kept on the bin clip for later placement and trimming.
  //
  // A first video into a fresh project sets the timeline format from its own (auto-adopt); later clips —
  // and audio clips, which carry no video format to adopt — are conformed to whatever the timeline already
  // is. The audio rate is never adopted (resampling is transparent). Adding to the bin is a plain project
  // edit, so it rides the ordinary re-render; there is no player work, nothing new being on the timeline.
  def importClip(pathStr: String, kind: MediaKind): Unit =
    val base =
      if kind == MediaKind.Video && project.shouldAdoptSpec then
        project.withSpec(Player.probeSpec(pathStr).copy(audioRate = project.spec.audioRate))
      else project
    val len  = Player.mediaLength(pathStr, base.spec)
    val clip = MediaClip.make(pathStr, kind, len)
    dirty.current = true
    editProject(_ => base.copy(bin = base.bin :+ clip))

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
    if selectedBinId.contains(id) then showProjectMonitor()
    val p = project.copy(
      bin = project.bin.filterNot(_.id == id),
      tracks = project.tracks.map(t => t.copy(clips = t.clips.filterNot(_.clipId == id))),
    )
    reopen(p, path)

  // Remove a placed clip from the timeline, taking its linked partner with it (a locked pair leaves as
  // one), and keeping the source in the bin so it can be placed again. Changing what is on the tracks
  // changes the lanes' generators, so this re-opens the player.
  def removePlacement(id: String): Unit =
    dirty.current = true
    val group = project.moveGroupOf(id).map(_._2.id).toSet
    val p     = project.copy(tracks = project.tracks.map(t => t.copy(clips = t.clips.filterNot(c => group.contains(c.id)))))
    setSelectedClipId(None)
    reopen(p, path)

  // Place a bin clip onto the timeline at the playhead. The drop lands at the current frame, snapped
  // past any clip already there and trimmed to fit the gap, so a track stays a non-overlapping sequence
  // (see `Timeline.freePlacement`). A **video** clip drops as a linked A/V pair — picture on the first
  // video track, sound on the first audio track, at one start across both — so the pair reads and moves
  // as one; an **audio** clip drops on the first audio track alone. Placing changes the lanes'
  // generators, so this re-opens the player, as importing does. Nothing happens when the playhead has no
  // room (a length of 0). The source length is measured against the profile, so on the UI/main thread.
  def placeAtPlayhead(clip: MediaClip): Unit =
    val srcLen = Player.mediaLength(clip.path, project.spec)
    clip.kind match
      case MediaKind.Video =>
        (project.videoTracks.headOption, project.audioTracks.headOption) match
          case (Some(v), Some(a)) =>
            val (start, length) = Timeline.freePlacement(
              playheadRef.current, srcLen,
              v.clips.map(c => (c.timelineStart, c.length)),
              a.clips.map(c => (c.timelineStart, c.length)),
            )
            if length > 0 then
              val link = Some(s"lnk-${System.nanoTime()}")
              val p = project.copy(tracks = project.tracks.map { t =>
                if t.id == v.id || t.id == a.id then
                  t.copy(clips = t.clips :+ PlacedClip.make(clip.id, start, length, link = link))
                else t
              })
              dirty.current = true
              reopen(p, path)
          case _ => ()
      case MediaKind.Audio =>
        project.audioTracks.headOption.foreach { a =>
          val (start, length) = Timeline.freePlacement(
            playheadRef.current, srcLen, a.clips.map(c => (c.timelineStart, c.length)), Nil,
          )
          if length > 0 then
            val p = project.updateTrack(a.id)(t => t.copy(clips = t.clips :+ PlacedClip.make(clip.id, start, length)))
            dirty.current = true
            reopen(p, path)
        }

  // The media on the timeline as a comparable key — each track's kind and its placements (source path,
  // in-point, length, start) in order — so opening a project with the same media reuses the live
  // graph-swap and a different arrangement re-opens.
  def mediaKey(p: Project): Seq[Any] =
    p.tracks.map(t => (t.kind, t.ordered.map(pc => (p.clipFor(pc.clipId).map(_.path), pc.inPoint, pc.length, pc.timelineStart, pc.mc, pc.angle))))

  // Apply a project loaded from `loadedPath`: if it arranges the same media as the current project, the
  // live rebuild handles it (just swap the project state); otherwise everything the player built is tied
  // to the old arrangement, so re-open from scratch.
  def applyOpened(p: Project, loadedPath: String): Unit =
    dirty.current = false // freshly loaded from disk, so it matches its file
    resetView()
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

  // Remove a bin clip, confirming first — removing a source takes its timeline placements with it.
  def confirmRemoveClip(clip: MediaClip): Unit =
    setConfirm(Some(ConfirmSpec(
      "Remove clip?",
      s"Remove “${clip.name}” from the project? Its placements on the timeline go with it; the lower thirds are kept.",
      "Remove",
      () => removeBinClip(clip.id),
    )))

  // Select a lower third for the inspector (clearing any clip selection — the two are exclusive).
  def selectLt(id: String): Unit =
    setSelectedId(Some(id))
    setSelectedClipId(None)

  // Multicam: cutting one program picture between synced angles (cameras and title slides) while one
  // source's audio plays through. The MVP holds one group; these derive what the switcher shows — the
  // group, the angle on air at the playhead (for the highlight), and whether the bin has the two videos a
  // group needs.
  val mcGroup   = project.multicams.headOption
  val mcActive  = mcGroup.map(g => Multicam.activeAngleAt(project, g.id, playheadRef.current)).getOrElse(-1)
  val canMakeMc = project.bin.count(_.kind == MediaKind.Video) >= 2

  // Build a multicam program from the bin's videos: sync them by their audio (envelopes from the player's
  // waveform cache, best-effort — an ungenerated one leaves an angle at offset 0 to nudge), put the
  // switched program on its own dedicated tracks, and drop the plain placements those clips had on the
  // shared tracks so the program is the single home for that footage. Re-opens: tracks and generators change.
  def makeMulticam(): Unit =
    val vids = project.bin.filter(_.kind == MediaKind.Video)
    if vids.size >= 2 then
      // Envelopes to sync on: the live waveform when the clip is already placed, otherwise generated on
      // demand (bin clips are no longer auto-placed, so a group is usually built from unplaced sources).
      val envs = vids.map { c =>
        val live = playerRef.current match { case pl: Player => pl.waveFor(c.path); case null => null }
        val env = live match
          case w: Waveform if w.envelope.nonEmpty => w.envelope
          case _ => Waveform.envelopeOf(project.spec, c.path, if c.frames > 0 then c.frames else Player.mediaLength(c.path, project.spec))
        c.path -> env
      }.toMap
      val length      = math.max(1, if vids.head.frames > 0 then vids.head.frames else Player.mediaLength(vids.head.path, project.spec))
      val (g, vt, at) = Multicam.buildProgram(vids, envs, math.round(fps * 2).toInt, length)
      val angleIds    = vids.map(_.id).toSet
      val cleared     = project.tracks.map(t => t.copy(clips = t.clips.filterNot(c => angleIds.contains(c.clipId))))
      dirty.current = true
      reopen(project.copy(tracks = cleared :+ vt :+ at, multicams = project.multicams :+ g), path)

  // Cut the program to angle `i` at the playhead — the one action both a live switch (clicked while
  // playing) and a frame-precise one (scrubbed, then clicked) drive. Rides the live graph-swap: the cut
  // list changes so the graph rebuilds, but the sources and their generators do not, so no re-open.
  def switchMulticam(i: Int): Unit =
    mcGroup.foreach(g => editProject(p => Multicam.switchProgram(p, g.id, playheadRef.current, i)))

  // Add a title-slide angle to the group so the program can cut to a title. It takes the selected lower
  // third's words and look when one is selected, else a placeholder the user renames in the project file.
  def addMcTitle(): Unit =
    mcGroup.foreach { g =>
      val lt  = selectedId.flatMap(id => project.lowerThirds.find(_.id == id))
      val ang = Multicam.titleAngle(
        lt.map(_.name).filter(_.nonEmpty).getOrElse("Title"),
        lt.map(_.name).getOrElse("Title"), lt.map(_.title).getOrElse("Subtitle"),
        lt.map(_.styleId).getOrElse(project.styles.head.id), lt.flatMap(_.body))
      editProject(p => Multicam.addTitle(p, g.id, ang))
    }

  val multicamPanel =
    MulticamPanel(MulticamProps(mcGroup, mcActive, canMakeMc, () => makeMulticam(), i => switchMulticam(i), () => addMcTitle()))

  // The bin panel (left) — see [[BinPanel]]. Its data comes from the project; its actions reach back
  // through the project-op and import helpers.
  val binPanel = BinPanel(BinProps(
    bin           = project.bin,
    lowerThirds   = project.lowerThirds,
    selectedBinId = selectedBinId,
    selectedLtId  = selectedId,
    onNew         = () => requestNew(),
    onOpen        = () => doOpen(),
    onSave        = () => doSave(),
    onSettings    = () => openSettings(),
    onExport      = () => requestExport(),
    onImportVideo = () => doImportVideo(),
    onImportAudio = () => doImportAudio(),
    onImportLts   = () => doImportLowerThirds(),
    onAddLt       = () => addLowerThird(),
    onPreviewClip = showClipMonitor,
    onPlaceClip   = placeAtPlayhead,
    onRemoveClip  = confirmRemoveClip,
    onSelectLt    = selectLt,
    onRemoveLt    = removeLowerThird,
  ))

  // A centred hint shown over the black preview when there is no picture to show.
  def previewHint(msg: String): VNode =
    box(flex = 1)(
      col(mainAxisAlignment = MainAxisAlignment.Center, crossAxisAlignment = CrossAxisAlignment.Center)(
        text(msg, size = 16, color = theme.surfaceText),
      ),
    )

  // The video preview — the top of the player panel. In the project monitor it shows the assembled
  // timeline (a notice while opening, black before any media is imported); in the clip monitor it shows
  // the previewed bin clip (a prompt when none is picked). The bin is where media is imported, so the
  // project preview needs no prompt of its own.
  val preview =
    box(bg = Color.black, flex = 1)(
      if isClip then
        clipLayer match
          case l: VideoLayer => video(l, fit = VideoFit.Contain)
          case null          => previewHint("Select a clip in the bin")
      else
        layer match
          case l: VideoLayer                => video(l, fit = VideoFit.Contain)
          case null if hasContent(project)  => previewHint("Opening…")
          case null                         => box(flex = 1)(),
    )

  // One monitor tab in the player panel's header: a rounded label that reads as pressed when it is the
  // shown monitor.
  def monitorTab(label: String, active: Boolean, onClick: () => Unit): VNode =
    box(onClick = _ => onClick(), cursor = Cursor.Pointer, bg = if active then theme.surface else theme.background,
      radius = 6, padding = EdgeInsets.symmetric(horizontal = 14, vertical = 6))(
      text(label, size = 12, weight = FontWeight.Bold, color = if active then theme.surfaceText else theme.border),
    )

  // The clip/project monitor switcher across the top of the player panel. The Clip tab reopens the clip
  // monitor on the selected bin clip (or shows its prompt when none is selected); Project returns to the
  // timeline.
  val monitorTabs =
    box(bg = theme.background, padding = EdgeInsets.symmetric(horizontal = 8, vertical = 6))(
      row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 6)(
        monitorTab("Clip", isClip, () =>
          selectedBinId.flatMap(id => project.bin.find(_.id == id)) match
            case Some(clip) => showClipMonitor(clip)
            case None       => setMonitorMode(MonitorMode.Clip)),
        monitorTab("Project", !isClip, () => showProjectMonitor()),
      ),
    )

  // The player panel (centre): the monitor tabs over the preview and its transport, grouped as one card.
  val playerPanel =
    panel(flexN = 1)(
      col(crossAxisAlignment = CrossAxisAlignment.Stretch)(monitorTabs, preview, transport, multicamPanel),
    )

  // The inspector (right) — see [[InspectorPanel]]. The selection is resolved here (a clip and a lower
  // third are never selected at once) and passed in; edits funnel back through the callbacks.
  val selectedClip =
    selectedClipId.flatMap(id =>
      project.tracks.flatMap(_.clips).find(_.id == id).flatMap(pc => project.clipFor(pc.clipId).map((pc, _))))
  val selectedLt     = selectedId.flatMap(id => project.lowerThirds.find(_.id == id))
  val inspectorPanel =
    InspectorPanel(InspectorProps(selectedClip, selectedLt, project.styles, fps, editLt, removePlacement, unlinkPlacement))

  // The audio mixer (a pane in the right column) — see [[MixerPanel]]. The master strip mirrors the
  // transport's volume control (same value, same handler), so the two stay in lockstep.
  val mixerPanel = MixerPanel(MixerProps(project.audioTracks, volume, setTrackVolume, toggleMute, onVolume))

  // The timeline panel (bottom) — see [[TimelinePanel]]. It owns the whole editing surface: the
  // viewport (pan/zoom/follow), scrubbing, and dragging/trimming clips and lower thirds. App hands it
  // the live project and the timeline metrics it computes (length, fit span, frame rate), the current
  // selection, and the two shared refs it advances and reads — the playhead and the project player;
  // every edit funnels back through the callbacks. `resetToken` refits it to the whole timeline.
  val timelinePanel = TimelinePanel(TimelineProps(
    project             = project,
    total               = total,
    fitFrames           = fitFrames,
    fps                 = fps,
    selectedClipId      = selectedClipId,
    selectedLtId        = selectedId,
    resetToken          = resetToken,
    playheadRef         = playheadRef,
    playerRef           = playerRef,
    editProject         = editProject(_),
    editLt              = editLt(_, _),
    setSelectedClipId   = setSelectedClipId,
    setSelectedLtId     = setSelectedId,
    focusProjectMonitor = () => focusProjectMonitor(),
  ))

  // The editor body, Resolve-style: a top row of bin | player | inspector — split by draggable
  // gutters (a horizontal splitter, nested so the player sits between the bin and the inspector) —
  // over the track panel, the two divided by a draggable vertical splitter so the timeline area can
  // be sized. It starts compact.
  val topRow =
    splitter(axis = Axis.Horizontal, initial = 0.18, min = 0.12, max = 0.4)(
      binPanel,
      splitter(axis = Axis.Horizontal, initial = 0.74, min = 0.5, max = 0.86)(
        playerPanel,
        // The right column stacks the inspector over the audio mixer, split by a draggable gutter.
        splitter(axis = Axis.Vertical, initial = 0.58, min = 0.3, max = 0.8)(
          inspectorPanel,
          mixerPanel,
        ),
      ),
    )

  // The modal dialogs — see [[ConfirmDialog]], [[SettingsDialog]], [[ExportDialog]]. Their state lives
  // here; the components are presentational. The frame rate is editable only on an empty timeline.
  val confirmDialog = ConfirmDialog(ConfirmProps(confirm, () => setConfirm(None)))
  val settingsDialog = SettingsDialog(SettingsProps(
    open      = settingsOpen,
    draft     = settingsDraft,
    fpsLocked = hasContent(project),
    onChange  = updateSettingsDraft,
    onApply   = () => applySettings(),
    onClose   = () => setSettingsOpen(false),
  ))
  val exportDialog = ExportDialog(ExportProps(exporting, exportProgress, () => cancelExport()))

  ThemeProvider(theme)(
    box(bg = theme.background, padding = EdgeInsets.all(8))(
      splitter(axis = Axis.Vertical, initial = 0.6, min = 0.35, max = 0.9)(
        topRow,
        timelinePanel,
      ),
      confirmDialog,
      settingsDialog,
      exportDialog,
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

  // What the app opens with — resolved from the argument (a `.kutter` project, a bare media path, or
  // nothing) against the remembered session; see [[Session.resolve]]. Needs MLT initialised (done
  // above) since a media argument is probed for its length and format.
  val session = Session.resolve(args, project)

  Suit.run("kutter", 1100, 720, maximized = true)(App(session))
  Mlt.close()
