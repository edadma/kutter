package io.github.edadma.kutter

import io.github.edadma.suit.*
import io.github.edadma.suit.dsl.*
import io.github.edadma.suit.widgets.*

// The timeline panel (the editor's bottom pane): the pinned time ruler over a vertical scroll of track
// lanes — one per project track (a video track's filmstrips and title cards, an audio track's
// waveforms). It owns the whole editing surface: scrubbing, panning and zooming the viewport, and
// dragging/trimming placed clips (a lower-third title placed on a video track is a clip like any other).
// Extracted from `App`, which keeps the two shared refs the panel reads — the timeline playhead and the
// project player — and passes them in; every project edit funnels back through the
// `editProject`/`editLt` callbacks.
//
// The viewport (which frame sits at the left edge, and the pixels-per-frame zoom) and every in-flight
// drag live in refs local to this component, so a pan, a zoom, or a playhead advance repaints the
// canvases without re-rendering the tree. The playhead is a *shared* ref because the transport's scrub
// and the bin's "place at playhead" also read and move it; this panel advances it during playback — and
// follows it, paging the view — from its own poll. `resetToken` is bumped by `App` (New / Open / a
// settings change) to refit the view to the whole timeline.
private final case class TimelineProps(
    project:             Project,
    total:               Int,
    fitFrames:           Int,
    fps:                 Double,
    selectedClipId:      Option[String],
    selectedLtId:        Option[String],
    resetToken:          Int,
    playheadRef:         Ref[Int],
    playerRef:           Ref[Player | Null],
    editProject:         (Project => Project) => Unit,
    setSelectedClipId:   Option[String] => Unit,
    setSelectedLtId:     Option[String] => Unit,
    focusProjectMonitor: () => Unit,
    onDropClip:          (String, String, Int) => Unit,
    onRemovePlacement:   String => Unit,
    onRemoveLowerThird:  String => Unit,
    onAddTrack:          MediaKind => Unit,
    onAddAvTracks:       () => Unit,
    onCut:               () => Unit,
    onUndo:              () => Unit,
    onRedo:              () => Unit,
    canUndo:             Boolean,
    canRedo:             Boolean,
    onDragBegin:         () => Unit,
    onDragEnd:           () => Unit,
)

private val TimelinePanel: Component[TimelineProps] = component[TimelineProps] { p =>
  val theme = useTheme()

  // The props the copied editing logic reads by bare name — the live project and the timeline metrics
  // App computes (length, the "fit" span, the frame rate) plus the shared refs and edit callbacks.
  val project           = p.project
  val total             = p.total
  val fps               = p.fps
  val playheadRef       = p.playheadRef
  val playerRef         = p.playerRef
  val editProject       = p.editProject
  val selectedClipId    = p.selectedClipId
  val selectedId        = p.selectedLtId
  val setSelectedClipId = p.setSelectedClipId
  val setSelectedId     = p.setSelectedLtId
  def focusProjectMonitor(): Unit = p.focusProjectMonitor()

  // Scrubbing state. While the pointer is held on the ruler or a lane, playback is paused and the
  // position follows the cursor; `wasPlaying` remembers whether to resume on release. Refs, not state,
  // because the drag handlers and the poll read them imperatively without needing a re-render. Separate
  // from the transport's own scrub state — a paused player can't fight this panel's poll.
  val scrubbing  = useRef(false)
  val wasPlaying = useRef(false)

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

  // The drop preview while a bin clip is dragged over a lane: the target track id and the (start, length)
  // the clip would land at, or null when nothing is being dragged over a lane. The lane paints a
  // translucent ghost from it, so the drop lands exactly where the ghost showed. A ref — updated on every
  // drag-over and cleared on leave/drop, driving a repaint, never a re-render.
  val dropPreview = useRef[(String, Int, Int) | Null](null)

  // Refit the viewport whenever App bumps the reset token (a new or newly opened project, or a settings
  // change): pan home and drop the zoom so the next paint fits the whole timeline to the window rather
  // than inheriting the last framing. Done here in the render body, synchronously before the canvases
  // paint, so the refit lands on the very next paint with no stale frame.
  val lastReset = useRef(p.resetToken)
  if p.resetToken != lastReset.current then
    lastReset.current = p.resetToken
    viewStart.current = 0.0
    viewPpf.current   = 0.0

  // Advance the timeline playhead from the project player's position and ask for a repaint — but only
  // when the frame has actually moved, so a paused editor asks for no repaints and the render loop
  // stays idle. This never re-renders the editor (no state write); it drives the timeline lanes, which
  // read `playheadRef` while being repainted. Skipped while a timeline scrub owns the position. ~30/s
  // for a smooth playhead without the churn of reconciling the tree.
  useInterval(
    () =>
      if !scrubbing.current then
        playerRef.current match
          case pl: Player =>
            val pos = pl.position
            if pos != playheadRef.current then
              playheadRef.current = pos
              // Follow playback: when the playhead runs off the visible window, flip the view so it
              // lands at the left edge and playback keeps scrolling into fresh timeline — the page
              // scroll every editor does. Only while playing, so a paused pan is never yanked back.
              if pl.isPlaying && viewPpf.current > 0 && viewWidth.current > 0 then
                val visible = viewWidth.current / viewPpf.current
                if pos < viewStart.current || pos > viewStart.current + visible then
                  viewStart.current = math.max(0.0, pos.toDouble)
              Repaint.request()
          case null => (),
    33,
  )

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
    if viewPpf.current <= 0 && width >= 100 then viewPpf.current = width / p.fitFrames
    val ppf = if viewPpf.current > 0 then viewPpf.current else math.max(1e-6, width / p.fitFrames)
    Timeline.View(viewStart.current, ppf)

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
      viewPpf.current   = viewWidth.current / p.fitFrames
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

  // Begin a scrub (from the timeline): pause and hold, remembering whether to resume, and jump to
  // `frame`. Touching the timeline pulls focus back to the project monitor if the clip monitor was
  // showing.
  def beginScrub(frame: Int): Unit =
    focusProjectMonitor()
    playerRef.current match
      case pl: Player =>
        scrubbing.current  = true
        wasPlaying.current = pl.isPlaying
        pl.pause()
        playheadRef.current = frame
        Repaint.request()
        pl.seek(frame)
      case null => ()

  // Continue a scrub: track the cursor to `frame`.
  def scrubTo(frame: Int): Unit =
    playheadRef.current = frame
    Repaint.request()
    playerRef.current match
      case pl: Player => pl.seek(frame)
      case null       => ()

  // End a scrub: resume if playback was running when it began.
  def endScrub(): Unit =
    scrubbing.current = false
    if wasPlaying.current then
      playerRef.current match
        case pl: Player => pl.play()
        case null       => ()

  // The edit points a drag's magnetism sticks to: every other clip's edges on every track (a title
  // placement's edges among them), the playhead, and the timeline origin — the targets any NLE snaps a
  // sliding block to. The dragged group's own blocks are excluded so a block never sticks to where it
  // already was. Snapshotted at grab time, like the rest of the drag state.
  def snapTargetsFor(excludeClips: Set[String]): Seq[Int] =
    val clipEdges = project.tracks.flatMap(_.clips).filterNot(c => excludeClips(c.id))
      .flatMap(c => Seq(c.timelineStart, c.timelineEnd))
    (clipEdges :+ playheadRef.current :+ 0).distinct

  // Select a placed clip for the inspector, clearing any lower-third selection (the two are exclusive).
  def selectClip(id: String): Unit =
    setSelectedClipId(Some(id))
    setSelectedId(None)

  // Begin dragging the clip `id`, grabbed at `grabFrame`: select it, resolve its move group (the pair,
  // when linked), snapshot each member's start, and compute how far the group may slide in either
  // direction — the tightest bound across every member, so a linked pair never lets one half overlap a
  // neighbour while the other moves. A press that doesn't move stays a plain selection.
  def beginClipDrag(id: String, grabFrame: Int): Unit =
    focusProjectMonitor()
    p.onDragBegin() // a whole move is one undo step, not one per frame
    selectClip(id)
    val group = project.moveGroupOf(id)
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
      cdragSnaps.current = snapTargetsFor(ids)
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
          editProject(pr =>
            pr.copy(tracks = pr.tracks.map(t =>
              t.copy(clips = t.clips.map(c =>
                starts.get(c.id) match
                  case Some(orig) => c.copy(timelineStart = orig + delta)
                  case None       => c,
              )),
            )),
          )
      case null => ()

  // End a clip drag.
  def endClipDrag(): Unit =
    if cdragId.current != null then p.onDragEnd()
    cdragId.current = null

  // The source length a placement can be trimmed against: the measured source frame count, or — for a
  // clip from a project saved before lengths were measured — the placement's own end, so an old clip
  // trims (shrinks) but isn't extended past what it already shows.
  def srcLenOf(pc: PlacedClip): Int =
    if pc.titleId.isDefined then total // a title card has no source; it can be any length up to the timeline
    else project.clipFor(pc.clipId).map(c => if c.frames > 0 then c.frames else pc.inPoint + pc.length)
      .getOrElse(pc.inPoint + pc.length)

  // Begin trimming the `edge` of clip `id`, grabbed at `grabFrame`: select it, resolve its trim group
  // (the linked pair, if any), snapshot each member's window, and compute the feasible edge delta — the
  // tightest bound across every member, so a linked pair trims together without either half running off
  // the source or over a neighbour.
  def beginTrim(id: String, edge: Timeline.TrimEdge, grabFrame: Int): Unit =
    focusProjectMonitor()
    p.onDragBegin() // a whole trim is one undo step, not one per frame
    selectClip(id)
    val group = project.moveGroupOf(id)
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
      tdragSnaps.current = snapTargetsFor(group.map(_._2.id).toSet)
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
          editProject(pr =>
            pr.copy(tracks = pr.tracks.map(t =>
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
  def endTrim(): Unit =
    if tdragId.current != null then p.onDragEnd()
    tdragId.current = null

  // The lane colour for a lower third, drawn from its style so a block on the timeline reads as the
  // look it wears: the accent stripe if the style has one, else the bar if it is solid enough, else
  // the theme accent (for a bar-less style like "minimal", whose card is all text).
  def blockColor(styleId: String): Color =
    val st  = project.styleFor(styleId)
    val src = st.stripe.getOrElse(st.bar)
    if src.a < 0.35 then theme.accent
    else Color((src.r * 255).round.toInt, (src.g * 255).round.toInt, (src.b * 255).round.toInt)

  // A project track's placed clips as timeline blocks. The geometry — where each block sits and how long
  // it is — is the live project, so a clip is drawn where it is placed and follows a drag at once; its
  // filmstrip or waveform is looked up from the player by source, so several placements of one clip share
  // a generator and an empty track simply draws no blocks. With no player open the blocks draw flat (no
  // generators yet) but still show at their positions. A title placement (a lower third on a video track)
  // draws as a flat block tinted by its style, with the title's name as the label.
  def blocksFor(t: Track): Seq[Timeline.ClipBlock] =
    t.ordered.flatMap { pc =>
      pc.titleId.flatMap(project.titleFor) match
        case Some(lt) =>
          Some(Timeline.ClipBlock(
            id         = pc.id,
            start      = pc.timelineStart,
            length     = pc.length,
            label      = if lt.name.nonEmpty then lt.name else lt.title,
            linked     = false,
            selected   = selectedClipId.contains(pc.id),
            titleColor = blockColor(lt.styleId),
          ))
        case None =>
          project.clipFor(pc.clipId).map { clip =>
            val (thumbs, wave) = playerRef.current match
              case pl: Player =>
                t.kind match
                  case MediaKind.Video => (pl.thumbsFor(clip.path), null)
                  case MediaKind.Audio => (null, pl.waveFor(clip.path))
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

  // The length to give a dropped title, which has no intrinsic source length — three seconds at the
  // timeline's rate, trimmed afterwards on the timeline like any clip.
  def defaultTitleFrames: Int = math.max(1, (3 * fps).toInt)

  // The source length a dragged payload lands with, and whether it may land on this track: a bin clip's
  // measured length on a matching-kind track, or a title's default length on a video track. `None` means
  // the payload cannot land here (an audio clip over a video lane, a title over an audio lane), so no
  // ghost shows and no drop happens.
  def payloadLen(payloadId: String, pt: Track): Option[Int] =
    project.titleFor(payloadId) match
      case Some(_) => Option.when(pt.kind == MediaKind.Video)(defaultTitleFrames)
      case None    => project.clipFor(payloadId).collect { case c if c.kind == pt.kind => math.max(1, c.frames) }

  // While a bin payload (a clip or a title) is dragged over a lane, resolve where it would land on that
  // track and remember it so the lane can paint a ghost — the same `freePlacement` math the drop itself
  // uses, so the preview and the result agree. A payload that cannot land here shows no ghost.
  def dragOverLane(pt: Track, e: DragEvent): Unit =
    val landing: (String, Int, Int) | Null =
      (e.payload match { case s: String => payloadLen(s, pt); case _ => None }) match
        case Some(srcLen) =>
          val f               = Timeline.frameAt(e.localX, total, viewFor(e.size.width))
          val (start, length) = Timeline.freePlacement(f, srcLen, pt.clips.map(c => (c.timelineStart, c.length)), Nil)
          if length > 0 then (pt.id, start, length) else null
        case None => null
    dropPreview.current = landing
    Repaint.request()

  // A bin payload dropped on a lane: hand the payload, the track, and the frame back to App to place (App
  // resolves a video clip into a linked A/V pair, a title into a card placement). Clear the ghost either way.
  def dropOnLane(pt: Track, e: DragEvent): Unit =
    e.payload match
      case s: String => p.onDropClip(s, pt.id, Timeline.frameAt(e.localX, total, viewFor(e.size.width)))
      case _         => ()
    dropPreview.current = null
    Repaint.request()

  def clearDropPreview(): Unit =
    dropPreview.current = null
    Repaint.request()

  // Paint the drop-preview ghost on track `trackId`'s lane, if a clip is being dragged over it: a
  // translucent accent block outlined in the accent across the frames the drop would occupy.
  def paintDropGhost(cv: Canvas, size: Size, trackId: String): Unit =
    dropPreview.current match
      case (tid, start, length) =>
        if tid == trackId && length > 0 then
          val view = viewFor(size.width)
          val x0   = view.xOf(start.toDouble)
          val w    = math.max(1.0, view.xOf((start + length).toDouble) - x0)
          val rect = Rect(x0, 2, w, size.height - 4)
          cv.fillRect(rect, theme.accent.withAlpha(70))
          cv.strokeRect(rect, theme.accent, 1.5)
      case null => ()

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

  // A small zoom control for the ruler's corner: a filled, rounded, bordered chip so it reads as a
  // button, acting on press. Kept compact (three sit in the narrow label column).
  def zoomButton(glyph: String, act: () => Unit): VNode =
    box(onClick = _ => act(), cursor = Cursor.Pointer, bg = theme.background, radius = 4,
      border = theme.border, borderWidth = 1, padding = EdgeInsets.symmetric(horizontal = 4, vertical = 1))(
      text(glyph, size = 10, weight = FontWeight.Bold, color = theme.surfaceText),
    )

  // The time ruler, pinned across the top of the track panel. Its left is the width of the track
  // labels and carries the zoom controls — out, fit the whole timeline, in — so the ruler's time
  // area starts where the track lanes do and the playhead lines up across all of them.
  val ruler =
    box(height = Timeline.RulerHeight)(
      row(crossAxisAlignment = CrossAxisAlignment.Stretch)(
        sizedBox(width = Timeline.LabelWidth)(
          box(bg = theme.surface)(
            row(mainAxisAlignment = MainAxisAlignment.Center, crossAxisAlignment = CrossAxisAlignment.Center, spacing = 3)(
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

  // One track widget: a left name column beside its lane canvas, which paints that track's clips (and
  // title cards) and its segment of the playhead. A left press near a clip's edge begins a trim, on its
  // body selects it and begins a move (a linked pair does both together), on the empty lane scrubs. The
  // edge is checked first so it stays grabbable even on a narrow clip. A middle press hand-pans the view;
  // the wheel pans too. The lane is a drop target for a bin payload dragged onto it (the ghost shows
  // where it will land).
  def trackWidget(t: Timeline.Track, pt: Track): VNode =
    val paint = (cv: Canvas, size: Size) =>
      Timeline.paintTrack(cv, size, t, viewFor(size.width), playheadRef.current, theme)
      paintDropGhost(cv, size, pt.id)
    val lane =
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
    box(height = Timeline.TrackHeight)(
      row(crossAxisAlignment = CrossAxisAlignment.Stretch)(
        trackLabel(t.name),
        box(flex = 1, onDragOver = e => dragOverLane(pt, e), onDragLeave = () => clearDropPreview(), onDrop = e => dropOnLane(pt, e))(lane),
      ),
    )

  // Delete or Backspace removes the current selection from the timeline — the selected clip (with its
  // linked partner), or else the selected lower third. The panel is focusable and a press anywhere in it
  // takes focus (pointer focus climbs to the nearest focusable ancestor), so clicking a clip both selects
  // it and focuses the panel; the key then reaches here. A text field editing an overlay's words keeps its
  // own focus, so Backspace there still edits text rather than deleting a clip.
  def onKey(e: KeyEvent): Unit =
    val primary = e.meta || e.ctrl // ⌘ on macOS, Ctrl elsewhere
    if primary && e.scancode == Key.Z then (if e.shift then p.onRedo() else p.onUndo())
    else if e.scancode == Key.Delete || e.scancode == Key.Backspace then
      selectedClipId match
        case Some(id) => p.onRemovePlacement(id)
        case None     => selectedId.foreach(p.onRemoveLowerThird)
    else if e.scancode == Key.S then p.onCut() // the razor: cut every clip at the playhead

  // The track panel: a card with the pinned ruler over a vertical scroll view of the track widgets, so
  // a tall stack scrolls while the ruler stays put. Each track is its own widget inside the panel rather
  // than one canvas painting them all.
  box(flex = 1, focusable = true, onKeyDown = onKey)(
   KutterUi.panel(theme)(1)(
    col(crossAxisAlignment = CrossAxisAlignment.Stretch)(
      ruler,
      box(flex = 1)(
        scrollView(axis = Axis.Vertical, scrollbar = true, scrollbarThumb = theme.border)(
          col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min)(
            // Lanes grouped by camera: ordered by track number, and within a number the video lane over its
            // audio lane — so a pair reads together (V1 above A1, then V2 above A2, …) and a freshly added
            // pair lands at the bottom, next to each other, rather than being split to opposite ends. Each
            // lane is a drop target that knows its own id and kind.
            project.tracks.sortBy(t => (t.num.getOrElse(Int.MaxValue), if t.kind == MediaKind.Video then 0 else 1))
              .map(pt => trackWidget(Timeline.Track(pt.name, blocksFor(pt)), pt))*,
          ),
        ),
      ),
      // Add another track. + A/V adds a paired video+audio lane (a camera's worth in one click, sharing a
      // number so a dropped clip's sound pairs to it); + Video / + Audio add a single lane. A new video
      // track stacks atop the video group; a new audio track goes below.
      box(bg = theme.background, padding = EdgeInsets.symmetric(horizontal = 8, vertical = 6))(
        row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 6)(
          // The razor: cut every clip at the playhead (also the S key when the timeline has focus).
          KutterUi.textButton(theme)("Cut", () => p.onCut()),
          // Undo / redo the last edit (also ⌘Z / ⌘⇧Z when the timeline has focus). Dimmed when empty.
          KutterUi.textButton(theme)("Undo", () => p.onUndo(), enabled = p.canUndo),
          KutterUi.textButton(theme)("Redo", () => p.onRedo(), enabled = p.canRedo),
          spacer(),
          KutterUi.textButton(theme)("+ A/V", () => p.onAddAvTracks()),
          KutterUi.textButton(theme)("+ Video", () => p.onAddTrack(MediaKind.Video)),
          KutterUi.textButton(theme)("+ Audio", () => p.onAddTrack(MediaKind.Audio)),
        ),
      ),
    ),
   ),
  )
}
