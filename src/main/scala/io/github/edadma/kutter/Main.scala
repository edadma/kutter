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

// How far the timeline runs past its content, as a floor in frames (a minute) — the tail of empty
// space that lets a clip be slid rightward, a drop land past the end, and material be placed well
// beyond what is already there. The actual tail is this or the whole content again, whichever is
// larger, so runway scales with the project; it costs nothing on screen (the view has a fixed
// scale and simply pans), and placing something in it grows the timeline further, so the runway
// never runs out. See where `total` is computed.
private val TimelineTailFrames = 1800

// A pending confirmation: the dialog shows its title and message, and runs `action` on confirm. One
// state drives every destructive prompt (remove a clip, clear the project), rather than a flag each.
private final case class ConfirmSpec(title: String, message: String, confirmLabel: String, action: () => Unit)

// Which monitor the centre panel shows, and which player the transport drives. The project monitor plays
// the assembled timeline; the clip monitor previews a single bin clip in isolation (as in kdenlive). Only
// one is engaged at a time — switching to the clip monitor pauses the project player and opens a light
// one-clip player; switching back closes it — so at most one player voices audio.
private enum MonitorMode:
  case Project, Clip

/** Format a frame count as m:ss.cc at the given frame rate — see [[KutterUi.timecode]]. */
private def timecode(frames: Int, fps: Double): String = KutterUi.timecode(frames, fps)

// Everything the transport needs from the editor, as a plain bundle. `player` reads the monitor the
// transport currently drives (the active one), so the transport can poll its position without the
// editor re-rendering; the callbacks reach back into the editor's playback state. `playing`, `total`,
// `label`, and `volume` are the values that change infrequently (a play/pause, a mode switch, a
// volume ride) — when they do, the editor re-renders and hands the transport a fresh bundle.
private final case class TransportProps(
    player:       () => Player | Null,
    total:        Int,
    fps:          Double,
    playing:      Boolean,
    label:        String,
    volume:       Double,
    onToggle:     () => Unit,
    onSeek:       Double => Unit,
    onScrubStart: () => Unit,
    onScrubEnd:   () => Unit,
    onVolume:     Double => Unit,
)

// The transport — the scrubber over play / timecode / frame index / name / master volume — as its own
// component, so its position readout ticking during playback re-renders only this small subtree
// (riposte re-renders the dirty instance, not the root) rather than the whole editor. It polls the
// active monitor's position itself, into local state, a few times a second; the editor drives the
// timeline playhead separately through a repaint (see `App`'s poll), so the expensive track lanes do
// not reconcile as the playhead moves.
private val Transport: Component[TransportProps] = component[TransportProps] { p =>
  val theme = Theme.dark

  // The polled position of the active monitor — the readout and the scrubber thumb. Held here, not in
  // the editor, so advancing it each tick re-renders only the transport.
  val (frame, setFrame, _) = useState(0)

  // While this scrubber owns the drag, the Slider shows the cursor directly, so the poll must not fight
  // it. A local ref (the editor has its own for timeline scrubs); no re-render needed to read it.
  val scrubbing = useRef(false)

  // The poll's timer is armed once and never re-armed (its deps are constant), so its callback would
  // otherwise close over the accessor from the first render and always read whichever monitor was
  // active then. Mirror the latest accessor into a ref each render so the poll reads the monitor the
  // transport currently drives — switching to the clip monitor then tracks the clip, not the project.
  val activeMonitor = useRef(p.player)
  activeMonitor.current = p.player

  useInterval(
    () =>
      if !scrubbing.current then
        activeMonitor.current() match
          case pl: Player => setFrame(pl.position)
          case null       => (),
    100,
  )

  val progress = if p.total > 0 then math.min(1.0, frame.toDouble / p.total) else 0.0

  def iconButton(icon: SvgImage, onClick: () => Unit): VNode =
    box(onClick = _ => onClick(), cursor = Cursor.Pointer, padding = EdgeInsets.all(8), radius = 8)(
      svg(icon, width = 22, height = 22),
    )

  val scrubber = Slider(
    value         = progress,
    onChange      = p.onSeek,
    onChangeStart = _ => { scrubbing.current = true; p.onScrubStart() },
    onChangeEnd   = _ => { scrubbing.current = false; p.onScrubEnd() },
  )

  box(bg = theme.surface, padding = EdgeInsets.symmetric(horizontal = 16, vertical = 10))(
    col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 10)(
      scrubber,
      row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 12)(
        iconButton(if p.playing then pauseIcon else playIcon, () => p.onToggle()),
        text(s"${timecode(frame, p.fps)} / ${timecode(p.total, p.fps)}", size = 14, color = theme.surfaceText, mono = true),
        text(s"frame $frame / ${p.total}", size = 12, color = theme.border, mono = true),
        spacer(),
        text(p.label, size = 13, color = theme.surfaceText),
        svg(volumeIcon, width = 18, height = 18),
        sizedBox(width = 90)(Slider(p.volume, p.onVolume)),
      ),
    ),
  )
}

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

  // Scrubbing state. While the pointer is held on the bar, playback is paused and the position
  // follows the cursor; `wasPlaying` remembers whether to resume on release. Refs, not state,
  // because the drag handlers and the poll read them imperatively without needing a re-render.
  val scrubbing  = useRef(false)
  val wasPlaying = useRef(false)

  // Dragging a title block along the timeline. On grab we remember which lower third (`dragId`), the
  // frame the cursor grabbed at (`dragGrab`), the block's window at that moment (`dragIn` /
  // `dragLen`), so each move places the block relative to the grab with no drift, and the edit points
  // the drag's magnetism sticks to (`dragSnaps`); `dragLastIn` skips a redundant edit when the frame
  // under the cursor hasn't changed. `dragId == null` means no drag.
  val dragId     = useRef[String | Null](null)
  val dragGrab   = useRef(0)
  val dragIn     = useRef(0)
  val dragLen    = useRef(0)
  val dragLastIn = useRef(0)
  val dragSnaps  = useRef[Seq[Int]](Nil)

  // Dragging a placed clip along its track. On grab we remember the clip (`cdragId`), the frame the
  // cursor grabbed at (`cdragGrab`), and the move group — the dragged clip plus, when it is one half of
  // a linked A/V pair, its partner on the other track — as (placement id, original start) pairs
  // (`cdragGroup`), so the whole group shifts by one delta and stays locked. `cdragLo`/`cdragHi` bound
  // that delta to what every member's track can accommodate without overlapping a neighbour;
  // `cdragLen` is the dragged clip's own length and `cdragSnaps` the edit points its magnetism sticks
  // to; `cdragLast` skips a redundant edit when the delta hasn't changed. `cdragId == null` means no
  // clip drag.
  val cdragId    = useRef[String | Null](null)
  val cdragGrab  = useRef(0)
  val cdragGroup = useRef[List[(String, Int)]](Nil)
  val cdragLo    = useRef(0)
  val cdragHi    = useRef(0)
  val cdragLen   = useRef(0)
  val cdragSnaps = useRef[Seq[Int]](Nil)
  val cdragLast  = useRef(0)

  // Trimming a placed clip's edge. On grab we remember the clip (`tdragId`), which edge (`tdragEdge`),
  // the frame grabbed at (`tdragGrab`), and the trim group — the clip plus a linked partner — as
  // (placement id, start, in-point, length) snapshots (`tdragGroup`), so a linked pair trims by one
  // delta and stays locked. `tdragLo`/`tdragHi` bound that delta to what stays on the source and in the
  // clip's gap (the tightest across the group); `tdragSnaps` holds the edit points the moving edge
  // sticks to; `tdragLast` skips a redundant edit. `tdragId == null` means no trim in progress.
  val tdragId    = useRef[String | Null](null)
  val tdragEdge  = useRef[Timeline.TrimEdge](Timeline.TrimEdge.Right)
  val tdragGrab  = useRef(0)
  val tdragGroup = useRef[List[(String, Int, Int, Int)]](Nil)
  val tdragLo    = useRef(0)
  val tdragHi    = useRef(0)
  val tdragSnaps = useRef[Seq[Int]](Nil)
  val tdragLast  = useRef(0)

  // The timeline viewport. `viewPpf` is the zoom — pixels per frame, fixed until the user zooms (0
  // until the first paint fits the whole timeline to the window); `viewStart` is the frame at the
  // lanes' left edge, moved by panning; `viewWidth` remembers the lane width from the last paint so
  // the zoom buttons and the playhead follow know the window size between paints. `panGrabX` /
  // `panOrigStart` carry a middle-button hand-pan. Refs, not state: the painters and handlers read
  // them imperatively — a pan or zoom repaints, never re-renders.
  val viewStart    = useRef(0.0)
  val viewPpf      = useRef(0.0)
  val viewWidth    = useRef(0.0)
  val panning      = useRef(false)
  val panGrabX     = useRef(0.0)
  val panOrigStart = useRef(0.0)

  // Advance the timeline playhead from the project player's position and ask for a repaint — but only
  // when the frame has actually moved, so a paused editor asks for no repaints and the render loop
  // stays idle. This never re-renders the editor (no state write); it drives the timeline lanes, which
  // read `playheadRef` while being repainted. Skipped while a timeline scrub owns the position. The
  // transport polls the active monitor's position on its own (see [[Transport]]). ~30/s for a smooth
  // playhead without the churn of reconciling the tree.
  useInterval(
    () =>
      if !scrubbing.current then
        playerRef.current match
          case p: Player =>
            val pos = p.position
            if pos != playheadRef.current then
              playheadRef.current = pos
              // Follow playback: when the playhead runs off the visible window, flip the view so it
              // lands at the left edge and playback keeps scrolling into fresh timeline — the page
              // scroll every editor does. Only while playing, so a paused pan is never yanked back.
              if p.isPlaying && viewPpf.current > 0 && viewWidth.current > 0 then
                val visible = viewWidth.current / viewPpf.current
                if pos < viewStart.current || pos > viewStart.current + visible then
                  viewStart.current = math.max(0.0, pos.toDouble)
              Repaint.request()
          case null => (),
    33,
  )

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
    (p.tracks.map(t => (t.kind, t.ordered.map(pc => (p.clipFor(pc.clipId).map(_.path), pc.inPoint, pc.length, pc.timelineStart)))),
     p.lowerThirds)

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
    case null      => (math.max(DefaultTimelineFrames, projectExtent), project.spec.fps)

  // The timeline's length: the content plus a tail of empty space, so a clip can always be slid right
  // (opening a gap before it for an intro) and a drop near the end has room. The tail grows with the
  // content — projectExtent tracks a moved clip — so within a project the runway never runs out; the
  // graph itself is only sized to the content (the black base ends at `contentReach`), so the tail is
  // purely timeline headroom, not rendered frames.
  val total    = contentReach + math.max(TimelineTailFrames, contentReach)

  // The span the "fit" framing shows: the content plus a little margin — not the pan runway, which
  // exists to give placements room, not to be looked at. An empty project fits its default window.
  val fitFrames = math.max(DefaultTimelineFrames, math.round(projectExtent * 1.05).toInt)

  // The timeline viewport for a lane `width` wide. The scale is set exactly once — the first paint
  // fits the whole timeline to the window, the familiar opening view — and after that it changes
  // only when the user zooms. Growing content therefore never rescales the lanes under the cursor;
  // the timeline extends past the window and the view pans across it. Every lane and the ruler share
  // one width (they sit beside the same label column), so one view serves them all.
  def viewFor(width: Double): Timeline.View =
    viewWidth.current = width
    // A layout pass can hand the canvas a degenerate width before the panels settle; latching the
    // fit to that would freeze a nonsense scale (a sliver-wide "whole timeline" the pan clamp then
    // pins in place). The fit waits for a real lane and earlier paints use a throwaway scale.
    if viewPpf.current <= 0 && width >= 100 then viewPpf.current = width / fitFrames
    val ppf = if viewPpf.current > 0 then viewPpf.current else math.max(1e-6, width / fitFrames)
    Timeline.View(viewStart.current, ppf)

  // Forget the viewport — pan home and refit the zoom to the window on the next paint. A new or
  // newly opened project starts from the whole-timeline view rather than inheriting the last
  // project's framing.
  def resetView(): Unit =
    viewStart.current = 0.0
    viewPpf.current   = 0.0

  // Keep the view's left edge on the timeline: never before frame 0, and never past the point where
  // a full window of timeline still shows (or 0 when the whole timeline fits the window).
  def clampViewStart(): Unit =
    val visible = if viewPpf.current > 0 then viewWidth.current / viewPpf.current else 0.0
    viewStart.current = math.max(0.0, math.min(total - visible, viewStart.current))

  // Zoom the timeline about the frame under `anchorPx`, so the spot under the cursor (or the window
  // centre, for the buttons) stays put while the scale changes around it. The scale is clamped
  // between a quarter of the fit scale (a wide overview) and a frame-filling close-up.
  def zoomTimelineAt(factor: Double, anchorPx: Double): Unit =
    if viewPpf.current > 0 && viewWidth.current > 0 then
      val anchor = viewStart.current + anchorPx / viewPpf.current
      val minPpf = viewWidth.current / total / 4
      val maxPpf = 12.0
      viewPpf.current   = math.max(minPpf, math.min(maxPpf, viewPpf.current * factor))
      viewStart.current = anchor - anchorPx / viewPpf.current
      clampViewStart()
      Repaint.request()

  // The ruler buttons' zoom: anchored on the window's centre.
  def zoomTimeline(factor: Double): Unit = zoomTimelineAt(factor, viewWidth.current / 2)

  // Reset the view to the content fitted to the window — the opening framing.
  def zoomFit(): Unit =
    if viewWidth.current > 0 then
      viewPpf.current   = viewWidth.current / fitFrames
      viewStart.current = 0.0
      Repaint.request()

  // A hand-pan: grab the timeline with the middle button anywhere on the ruler or a lane and slide
  // the view back and forth under the cursor.
  def beginPan(x: Double): Unit =
    panning.current      = true
    panGrabX.current     = x
    panOrigStart.current = viewStart.current

  def panTo(x: Double): Unit =
    if viewPpf.current > 0 then
      viewStart.current = panOrigStart.current - (x - panGrabX.current) / viewPpf.current
      clampViewStart()
      Repaint.request()

  def endPan(): Unit = panning.current = false

  // The wheel over the timeline. Plain, it pans horizontally (a notch is ~40px, the step suit's
  // scroll views use; a sideways trackpad swipe pans too). With the primary modifier held (Ctrl, or
  // ⌘ on a Mac) it zooms about the cursor instead — the universal editor convention, scrolling away
  // to zoom in. Consumed so the track stack's scroll view doesn't also act.
  def wheelPan(e: ScrollEvent): Unit =
    if viewPpf.current > 0 then
      if e.ctrl || e.meta then
        if e.deltaY != 0 then
          zoomTimelineAt(math.pow(1.15, e.deltaY), e.localX)
          e.consume()
      else
        val d = if math.abs(e.deltaX) > math.abs(e.deltaY) then e.deltaX else e.deltaY
        if d != 0 then
          viewStart.current -= d * 40.0 / viewPpf.current
          clampViewStart()
          Repaint.request()
          e.consume()

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

  // Begin a scrub (from the timeline): pause and hold, remembering whether to resume, and jump to
  // `frame`. Shares the drag state with the transport scrubber's brackets. Touching the timeline pulls
  // focus back to the project monitor if the clip monitor was showing.
  def beginScrub(frame: Int): Unit =
    focusProjectMonitor()
    playerRef.current match
      case p: Player =>
        scrubbing.current  = true
        wasPlaying.current = p.isPlaying
        p.pause()
        playheadRef.current = frame
        Repaint.request()
        p.seek(frame)
      case null => ()

  // Continue a scrub: track the cursor to `frame`.
  def scrubTo(frame: Int): Unit =
    playheadRef.current = frame
    Repaint.request()
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

  // The edit points a drag's magnetism sticks to: every other clip's edges on every track, every
  // other lower third's window, the playhead, and the timeline origin — the targets any NLE snaps a
  // sliding block to. The dragged group's own blocks are excluded so a block never sticks to where it
  // already was. Snapshotted at grab time, like the rest of the drag state.
  def snapTargetsFor(excludeClips: Set[String], excludeLts: Set[String]): Seq[Int] =
    val clipEdges = project.tracks.flatMap(_.clips).filterNot(c => excludeClips(c.id))
      .flatMap(c => Seq(c.timelineStart, c.timelineEnd))
    val ltEdges = project.lowerThirds.filterNot(lt => excludeLts(lt.id))
      .flatMap(lt => Seq(lt.inFrame, lt.outFrame))
    (clipEdges ++ ltEdges :+ playheadRef.current :+ 0).distinct

  // Begin dragging the title block `id`, grabbed at `grabFrame`: select it and snapshot its window
  // (and the edit points its magnetism sticks to) so the move is relative to the grab.
  def beginOverlayDrag(id: String, grabFrame: Int): Unit =
    focusProjectMonitor()
    setSelectedId(Some(id))
    setSelectedClipId(None)
    project.lowerThirds.find(_.id == id).foreach { lt =>
      dragId.current     = id
      dragGrab.current   = grabFrame
      dragIn.current     = lt.inFrame
      dragLen.current    = lt.outFrame - lt.inFrame
      dragLastIn.current = lt.inFrame
      dragSnaps.current  = snapTargetsFor(Set.empty, Set(id))
    }

  // Continue a title drag: follow the cursor frame for frame from the grab — sticking to a nearby
  // edit point when either edge comes within the magnet's reach — keeping the block's length and
  // clamping it within the timeline. Only edits when the target frame changes, so a press that
  // doesn't move stays a plain selection and drives no graph rebuild.
  def dragOverlay(curFrame: Int, reach: Int): Unit =
    dragId.current match
      case id: String =>
        val want    = curFrame - dragGrab.current
        val snapped = Timeline.snapDelta(want, dragIn.current, dragLen.current, dragSnaps.current, reach)
        val newIn   = Timeline.dragPlacement(dragIn.current, dragLen.current, snapped, total)
        if newIn != dragLastIn.current then
          dragLastIn.current = newIn
          editLt(id, _.copy(inFrame = newIn, outFrame = newIn + dragLen.current))
      case null => ()

  // End a title drag.
  def endOverlayDrag(): Unit = dragId.current = null

  // Select a placed clip for the inspector, clearing any lower-third selection (the two are exclusive).
  def selectClip(id: String): Unit =
    setSelectedClipId(Some(id))
    setSelectedId(None)

  // The clips that move together with the placement `id`: itself, and — when it is one half of a linked
  // A/V pair — every placement sharing its link id, across tracks. Each returned with the track it is on,
  // so a move can respect that track's neighbours. An unlinked clip moves alone.
  def moveGroupOf(p: Project, id: String): List[(String, PlacedClip)] =
    val target = p.tracks.flatMap(t => t.clips.find(_.id == id).map(c => (t.id, c))).headOption
    target match
      case Some((_, pc)) =>
        pc.link match
          case Some(lnk) => p.tracks.flatMap(t => t.clips.filter(_.link.contains(lnk)).map(c => (t.id, c)))
          case None      => target.toList
      case None => Nil

  // Begin dragging the clip `id`, grabbed at `grabFrame`: select it, resolve its move group (the pair,
  // when linked), snapshot each member's start, and compute how far the group may slide in either
  // direction — the tightest bound across every member, so a linked pair never lets one half overlap a
  // neighbour while the other moves. A press that doesn't move stays a plain selection.
  def beginClipDrag(id: String, grabFrame: Int): Unit =
    focusProjectMonitor()
    selectClip(id)
    val group = moveGroupOf(project, id)
    if group.nonEmpty then
      val ids = group.map(_._2.id).toSet
      var lo  = Int.MinValue
      var hi  = Int.MaxValue
      for (trackId, pc) <- group do
        val others = project.tracks.find(_.id == trackId).toList
          .flatMap(_.clips).filterNot(c => ids.contains(c.id)).map(c => (c.timelineStart, c.length))
        val (blo, bhi) = Timeline.clipStartBounds(pc.timelineStart, pc.length, total, others)
        lo = math.max(lo, blo - pc.timelineStart)
        hi = math.min(hi, bhi - pc.timelineStart)
      cdragId.current    = id
      cdragGrab.current  = grabFrame
      cdragGroup.current = group.map { case (_, pc) => (pc.id, pc.timelineStart) }
      cdragLo.current    = lo
      cdragHi.current    = hi
      cdragLen.current   = group.find(_._2.id == id).map(_._2.length).getOrElse(0)
      cdragSnaps.current = snapTargetsFor(ids, Set.empty)
      cdragLast.current  = 0

  // Continue a clip drag: shift the whole group by the cursor's travel from the grab, frame for
  // frame — sticking to a nearby edit point (a neighbouring clip's edge on any track, a title
  // window, the playhead) when either edge of the dragged clip comes within the magnet's reach —
  // clamped to the group's feasible delta, and move every member together. Only edits when the delta
  // changes, so a press that doesn't move drives no rebuild.
  def dragClip(curFrame: Int, reach: Int): Unit =
    cdragId.current match
      case id: String =>
        val want  = curFrame - cdragGrab.current
        val orig  = cdragGroup.current.find(_._1 == id).map(_._2).getOrElse(0)
        // Until the cursor has actually traveled, don't let the magnet move anything — a press that
        // merely selects must not nudge a clip resting near an edit point.
        val snapped = if want == 0 then 0 else Timeline.snapDelta(want, orig, cdragLen.current, cdragSnaps.current, reach)
        val delta = math.max(cdragLo.current, math.min(cdragHi.current, snapped))
        if delta != cdragLast.current then
          cdragLast.current = delta
          val starts = cdragGroup.current.toMap
          editProject(p =>
            p.copy(tracks = p.tracks.map(t =>
              t.copy(clips = t.clips.map(c =>
                starts.get(c.id) match
                  case Some(orig) => c.copy(timelineStart = orig + delta)
                  case None       => c,
              )),
            )),
          )
      case null => ()

  // End a clip drag.
  def endClipDrag(): Unit = cdragId.current = null

  // The source length a placement can be trimmed against: the measured source frame count, or — for a
  // clip from a project saved before lengths were measured — the placement's own end, so an old clip
  // trims (shrinks) but isn't extended past what it already shows.
  def srcLenOf(pc: PlacedClip): Int =
    project.clipFor(pc.clipId).map(c => if c.frames > 0 then c.frames else pc.inPoint + pc.length)
      .getOrElse(pc.inPoint + pc.length)

  // Begin trimming the `edge` of clip `id`, grabbed at `grabFrame`: select it, resolve its trim group
  // (the linked pair, if any), snapshot each member's window, and compute the feasible edge delta — the
  // tightest bound across every member, so a linked pair trims together without either half running off
  // the source or over a neighbour.
  def beginTrim(id: String, edge: Timeline.TrimEdge, grabFrame: Int): Unit =
    focusProjectMonitor()
    selectClip(id)
    val group = moveGroupOf(project, id)
    if group.nonEmpty then
      var lo = Int.MinValue
      var hi = Int.MaxValue
      for (trackId, pc) <- group do
        val others = project.tracks.find(_.id == trackId).toList
          .flatMap(_.clips).filterNot(c => group.exists(_._2.id == c.id)).map(c => (c.timelineStart, c.length))
        val (blo, bhi) = Timeline.clipTrimBounds(edge, pc.timelineStart, pc.inPoint, pc.length, srcLenOf(pc), total, others)
        lo = math.max(lo, blo)
        hi = math.min(hi, bhi)
      tdragId.current    = id
      tdragEdge.current  = edge
      tdragGrab.current  = grabFrame
      tdragGroup.current = group.map { case (_, pc) => (pc.id, pc.timelineStart, pc.inPoint, pc.length) }
      tdragLo.current    = lo
      tdragHi.current    = hi
      tdragSnaps.current = snapTargetsFor(group.map(_._2.id).toSet, Set.empty)
      tdragLast.current  = 0

  // Continue a trim: move the grabbed edge by the cursor's travel from the grab, frame for frame —
  // sticking to a nearby edit point when the edge comes within the magnet's reach — clamped to the
  // group's feasible delta, and apply it to every member — a right-edge trim changes the length, a
  // left-edge trim moves the in-point and start together (shortening from the head). Only edits when
  // the delta changes.
  def dragTrim(curFrame: Int, reach: Int): Unit =
    tdragId.current match
      case id: String =>
        val want = curFrame - tdragGrab.current
        val edge = tdragEdge.current
        val origEdge = tdragGroup.current.find(_._1 == id) match
          case Some((_, s, _, l)) => edge match
              case Timeline.TrimEdge.Right => s + l
              case Timeline.TrimEdge.Left  => s
          case None => 0
        val snapped = Timeline.snapEdgeDelta(want, origEdge, tdragSnaps.current, reach)
        val delta   = math.max(tdragLo.current, math.min(tdragHi.current, snapped))
        if delta != tdragLast.current then
          tdragLast.current = delta
          val origs = tdragGroup.current.map { case (pid, s, ip, l) => pid -> (s, ip, l) }.toMap
          editProject(p =>
            p.copy(tracks = p.tracks.map(t =>
              t.copy(clips = t.clips.map(c =>
                origs.get(c.id) match
                  case Some((s, ip, l)) =>
                    edge match
                      case Timeline.TrimEdge.Right => c.copy(length = l + delta)
                      case Timeline.TrimEdge.Left  => c.copy(timelineStart = s + delta, inPoint = ip + delta, length = l - delta)
                  case None => c,
              )),
            )),
          )
      case null => ()

  // End a trim.
  def endTrim(): Unit = tdragId.current = null

  // Unlink the clip `id` from its A/V partner: clear the shared link id on both halves so the picture
  // and its sound move (and later trim) independently. The graph is unchanged — the link is a timeline
  // grouping, not a render property — so this rides the live graph-swap like any other project edit.
  def unlinkPlacement(id: String): Unit =
    val group = moveGroupOf(project, id).map(_._2.id).toSet
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
        scrubbing.current  = true
        wasPlaying.current = p.isPlaying
        p.pause()
      case null => ()

  // End a transport scrub: resume if it was playing.
  def onScrubEnd(): Unit =
    scrubbing.current = false
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

  // Append `clip` to the end of `track` (a placement `len` frames long at the track's content end),
  // returning the updated project. Used to build up a track by importing clips.
  def appendTo(p: Project, trackId: String, clip: MediaClip, len: Int, link: Option[String]): Project =
    p.updateTrack(trackId)(t => t.copy(clips = t.clips :+ PlacedClip.make(clip.id, t.contentEnd, len, link = link)))

  // Import a clip of `kind` into the bin and place it, then re-open the player on it. Placing a clip
  // changes the timeline lanes (each owns a background generator), so this rebuilds the player from
  // scratch rather than the live graph-swap the lower-third edits use. The clip's length is measured
  // against the profile (a producer, so on the UI/main thread).
  //
  // A **video** clip is placed as a linked A/V pair — its picture on the first video track and its
  // audio on the first audio track, sharing a link id and the same window — so A1 shows the video's
  // peaks under the picture (for clapper/audio sync) and its sound plays from the audio track. An
  // **audio** clip goes on the first audio track alone. Stage 1 appends; dragging a bin clip to an
  // arbitrary track position, and unlinking a pair, come with the timeline UI.
  def importClip(pathStr: String, kind: MediaKind): Unit =
    dirty.current = true
    // A first video clip into a fresh project sets the timeline format from its own (auto-adopt); later
    // clips — and audio clips, which carry no video format to adopt — are conformed to whatever the
    // timeline already is. The audio rate is never adopted (resampling is transparent), so the project's
    // is kept. Measuring the clip's length happens against the resulting spec, so the length is in
    // timeline frames.
    val base =
      if kind == MediaKind.Video && project.shouldAdoptSpec then
        project.withSpec(Player.probeSpec(pathStr).copy(audioRate = project.spec.audioRate))
      else project
    val len      = Player.mediaLength(pathStr, base.spec)
    val clip     = MediaClip.make(pathStr, kind, len)
    val withClip = base.copy(bin = base.bin :+ clip)
    val placed = kind match
      case MediaKind.Video =>
        val link = Some(s"lnk-${System.nanoTime()}")
        val onV  = base.videoTracks.headOption.map(t => appendTo(withClip, t.id, clip, len, link)).getOrElse(withClip)
        base.audioTracks.headOption.map(t => appendTo(onV, t.id, clip, len, link)).getOrElse(onV)
      case MediaKind.Audio =>
        base.audioTracks.headOption.map(t => appendTo(withClip, t.id, clip, len, None)).getOrElse(withClip)
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
    val group = moveGroupOf(project, id).map(_._2.id).toSet
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
    p.tracks.map(t => (t.kind, t.ordered.map(pc => (p.clipFor(pc.clipId).map(_.path), pc.inPoint, pc.length, pc.timelineStart))))

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
      col(crossAxisAlignment = CrossAxisAlignment.Stretch)(monitorTabs, preview, transport),
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

  // A project track's placed clips as timeline blocks. The geometry — where each block sits and how long
  // it is — is the live project, so a clip is drawn where it is placed and follows a drag at once; its
  // filmstrip or waveform is looked up from the player by source, so several placements of one clip share
  // a generator and an empty track simply draws no blocks. With no player open the blocks draw flat (no
  // generators yet) but still show at their positions.
  def blocksFor(t: Track): Seq[Timeline.ClipBlock] =
    t.ordered.flatMap { pc =>
      project.clipFor(pc.clipId).map { clip =>
        val (thumbs, wave) = playerRef.current match
          case p: Player =>
            t.kind match
              case MediaKind.Video => (p.thumbsFor(clip.path), null)
              case MediaKind.Audio => (null, p.waveFor(clip.path))
          case null => (null, null)
        Timeline.ClipBlock(
          id       = pc.id,
          start    = pc.timelineStart,
          length   = pc.length,
          label    = clip.name,
          linked   = pc.link.isDefined,
          selected = selectedClipId.contains(pc.id),
          inPoint  = pc.inPoint,
          srcLen   = clip.frames,
          thumbs   = thumbs,
          waveform = wave,
        )
      }
    }

  // The tracks, drawn top-to-bottom: one lane per project track (a video track's filmstrips, an audio
  // track's waveforms), then the titles lane the lower thirds ride on. The empty default tracks (V1, A1)
  // still show as lanes waiting for clips, as in any editor. The titles lane is always present so it is a
  // stable target as overlays are added.
  val mediaTracks = project.tracks.map(t => Timeline.Track(t.name, blocksFor(t)))
  val tracks      = mediaTracks :+ Timeline.Track("Titles", overlays = overlayBlocks)

  // Scrubbing works from the ruler or any track: a left press begins, drag seeks (the canvas
  // captures the pointer so it tracks past the widget's edges), release ends. A middle press
  // hand-pans the view instead, and the wheel pans too. All widgets share the timeline width, so
  // one view serves every one.
  def scrubCanvas(paint: (Canvas, Size) => Unit): VNode =
    canvas(
      onMouseDown = e =>
        if e.button == 2 then beginPan(e.localX)
        else beginScrub(Timeline.frameAt(e.localX, total, viewFor(e.size.width))),
      onMouseMove = e =>
        if panning.current then panTo(e.localX)
        else if scrubbing.current then scrubTo(Timeline.frameAt(e.localX, total, viewFor(e.size.width))),
      onMouseUp = _ =>
        endPan()
        endScrub(),
      onWheel = wheelPan,
    )(paint)

  // A small zoom control for the ruler's corner: a boxed glyph that acts on press.
  def zoomButton(glyph: String, act: () => Unit): VNode =
    box(onClick = _ => act(), cursor = Cursor.Pointer, padding = EdgeInsets.symmetric(horizontal = 3, vertical = 0))(
      text(glyph, size = 11, weight = FontWeight.Bold, color = theme.border),
    )

  // The time ruler, pinned across the top of the track panel. Its left is the width of the track
  // labels and carries the zoom controls — out, fit the whole timeline, in — so the ruler's time
  // area starts where the track lanes do and the playhead lines up across all of them.
  val ruler =
    box(height = Timeline.RulerHeight)(
      row(crossAxisAlignment = CrossAxisAlignment.Stretch)(
        sizedBox(width = Timeline.LabelWidth)(
          box(bg = theme.surface)(
            row(mainAxisAlignment = MainAxisAlignment.Center, crossAxisAlignment = CrossAxisAlignment.Center)(
              zoomButton("−", () => zoomTimeline(1 / 1.5)),
              zoomButton("fit", () => zoomFit()),
              zoomButton("+", () => zoomTimeline(1.5)),
            ),
          ),
        ),
        box(flex = 1)(scrubCanvas((cv, size) => Timeline.paintRuler(cv, size, viewFor(size.width), playheadRef.current, fps, theme))),
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
    val paint = (cv: Canvas, size: Size) => Timeline.paintTrack(cv, size, t, viewFor(size.width), playheadRef.current, theme)
    val lane =
      t.overlays match
        case null =>
          // A media lane: a left press near a clip's edge begins a trim, on its body selects it and
          // begins a move (a linked pair does both together), on the empty lane scrubs. The edge is
          // checked first so it stays grabbable even on a narrow clip. A middle press hand-pans the
          // view; the wheel pans too.
          canvas(
            onMouseDown = e =>
              if e.button == 2 then beginPan(e.localX)
              else
                val view = viewFor(e.size.width)
                val f    = Timeline.frameAt(e.localX, total, view)
                Timeline.clipEdgeAt(e.localX, view, t.clips) match
                  case Some((id, edge)) => beginTrim(id, edge, f)
                  case None =>
                    Timeline.clipAt(e.localX, view, t.clips) match
                      case Some(id) => beginClipDrag(id, f)
                      case None     => beginScrub(f),
            onMouseMove = e =>
              if panning.current then panTo(e.localX)
              else
                val view  = viewFor(e.size.width)
                val f     = Timeline.frameAt(e.localX, total, view)
                val reach = Timeline.snapReach(view)
                if tdragId.current != null then dragTrim(f, reach)
                else if cdragId.current != null then dragClip(f, reach)
                else if scrubbing.current then scrubTo(f),
            onMouseUp = _ =>
              endPan()
              endTrim()
              endClipDrag()
              endScrub(),
            onWheel = wheelPan,
          )(paint)
        case blocks =>
          canvas(
            onMouseDown = e =>
              if e.button == 2 then beginPan(e.localX)
              else
                val view = viewFor(e.size.width)
                Timeline.overlayAt(e.localX, view, blocks) match
                  case Some(id) => beginOverlayDrag(id, Timeline.frameAt(e.localX, total, view))
                  case None     => beginScrub(Timeline.frameAt(e.localX, total, view)),
            onMouseMove = e =>
              if panning.current then panTo(e.localX)
              else
                val view = viewFor(e.size.width)
                val f    = Timeline.frameAt(e.localX, total, view)
                if dragId.current != null then dragOverlay(f, Timeline.snapReach(view))
                else if scrubbing.current then scrubTo(f),
            onMouseUp = _ =>
              endPan()
              endOverlayDrag()
              endScrub(),
            onWheel = wheelPan,
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
        trackPanel,
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
          .getOrElse {
            // A bare media argument starts a fresh project that adopts the clip's own format.
            val spec = Player.probeSpec(mediaArg)
            Session(Diagnostics.videoProject(mediaArg, Player.mediaLength(mediaArg, spec)).withSpec(spec), None)
          }
      case None =>
        SessionStore.load().getOrElse(Session(Project.blank, None))

  Suit.run("kutter", 1100, 720, maximized = true)(App(session))
  Mlt.close()
