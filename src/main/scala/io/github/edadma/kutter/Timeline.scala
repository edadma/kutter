package io.github.edadma.kutter

import io.github.edadma.suit.*

// The timeline: the editor's spine. It is not one widget but several — a pinned time ruler across the
// top and one widget per track below it — composed in `Main` into the track panel, with the tracks in
// a scroll view so a tall stack scrolls while the ruler stays put. This object holds the shared
// geometry and the per-widget painters; each track paints its own clips and its own segment of the
// playhead, and because every widget shares the same width and time mapping the segments line up into
// one continuous playhead. A lower third is not a lane of its own — it is placed on a video track like a
// clip and draws as a tinted title block among that track's clips.
object Timeline:

  val RulerHeight = 22.0 // the time-ruler widget's height
  val TrackHeight = 68.0 // one track widget's height
  val LabelWidth  = 60.0 // the track-name column down the left, as in any editor; the ruler leaves it blank

  /** The working timeline length (in frames at the 30fps profile) a project has before any footage is
    * placed, so lower thirds can be laid out ahead of the video — 10 seconds. */
  val DefaultTimelineFrames = 300

  /** How far the timeline runs past its content, as a floor in frames (a minute) — the tail of empty
    * space that lets a clip be slid rightward, a drop land past the end, and material be placed well
    * beyond what is already there. The actual tail is this or the whole content again, whichever is
    * larger, so runway scales with the project; it costs nothing on screen (the view has a fixed scale
    * and simply pans), and placing something in it grows the timeline further, so the runway never runs
    * out. See where `total` is computed in `App`. */
  val TimelineTailFrames = 1800

  /** Which edge of a clip block a trim drags: the left edge (moving the in-point and the start together)
    * or the right edge (moving the out-point, i.e. the length). */
  enum TrimEdge:
    case Left, Right

  /** One placed clip's block on a media lane: where it sits on the timeline (`start` for `length`
    * frames), the slice of the source it plays (`inPoint` into a source `srcLen` frames long), a
    * caption, whether it is one half of a linked A/V pair, whether it is the selected clip, and the
    * generator that fills it — a filmstrip (`thumbs`, on a video track) or a waveform overview
    * (`waveform`, on an audio track). A block carries whichever one its track draws. The generator spans
    * the whole source and is keyed by source, so several placements of the same clip share one; `inPoint`
    * and `srcLen` map the block's span back onto that whole-source generator, so a trimmed block shows
    * exactly the slice it plays. `srcLen` of 0 (a source whose length was never measured) falls back to a
    * one-to-one block mapping. The block's geometry is the live placement, so a clip drawn here always
    * sits where the project puts it. */
  case class ClipBlock(
      id:         String,
      start:      Int,
      length:     Int,
      label:      String,
      linked:     Boolean,
      selected:   Boolean,
      inPoint:    Int              = 0,
      srcLen:     Int              = 0,
      thumbs:     Thumbnails | Null = null,
      waveform:   Waveform | Null   = null,
      titleColor: Color | Null      = null, // a lower-third placement: draw a flat block in this tint, not a filmstrip/waveform
  )

  /** One track: a named lane sequencing its placed `clips` at their timeline positions — each a
    * filmstrip (a video clip), a waveform (an audio clip), or a flat tinted block (a lower-third title
    * placed on a video track). */
  case class Track(
      name:  String,
      clips: Seq[ClipBlock] = Nil,
  )

  /** The timeline viewport: which frame sits at the widget's left edge (`start`, fractional so a
    * pan is pixel-smooth) and how many pixels one frame occupies (`pxPerFrame`). Every frame↔x
    * conversion goes through a View, and the scale is *fixed* — it changes only when the user zooms,
    * never because content grew. That is the difference between an editor timeline and a squeezed
    * overview: content wider than the window pans across it instead of everything rescaling
    * ("scrunching") to fit. */
  case class View(start: Double, pxPerFrame: Double):
    /** The widget-local x where `frame` falls. */
    def xOf(frame: Double): Double = (frame - start) * pxPerFrame

    /** The (fractional) frame under widget-local x `px`. */
    def frameAtPx(px: Double): Double = start + px / math.max(1e-9, pxPerFrame)

  /** The frame under widget-local x `px`, clamped to the timeline. Turns a cursor position into a
    * seek. */
  def frameAt(px: Double, total: Int, view: View): Int =
    val f = math.round(view.frameAtPx(px)).toInt
    math.max(0, math.min(total - 1, f))

  /** The magnet's reach: a fixed 8 screen pixels expressed as frames under the view's scale — how
    * close a dragged edge must come to an edit point before it sticks — and at least one frame, so
    * the magnet still exists zoomed far in. */
  def snapReach(view: View): Int =
    math.max(1, math.round(8.0 / math.max(1e-9, view.pxPerFrame)).toInt)

  /** Magnetic snapping for a sliding block, the way any editor's timeline behaves: the block follows
    * the cursor frame for frame, and when either of its edges comes within `reach` frames of one of
    * the `targets` — the edit points: other clips' edges on every track, the title windows, the
    * playhead, the timeline origin — it sticks to that point exactly. Takes the frame-accurate
    * `delta` the cursor asks for (for a block at `origStart` of `length` frames) and returns it
    * adjusted by the nearest such stick, or unchanged when nothing is near. */
  def snapDelta(delta: Int, origStart: Int, length: Int, targets: Seq[Int], reach: Int): Int =
    val start = origStart + delta
    val fixes = targets.iterator.flatMap(t => Iterator(t - start, t - (start + length)))
    delta + fixes.filter(f => math.abs(f) <= reach).minByOption(math.abs).getOrElse(0)

  /** The same magnetism for a single sliding edge — a trim: the cursor's `delta` for an edge at
    * `origEdge`, adjusted so the edge sticks to the nearest target within `reach`. */
  def snapEdgeDelta(delta: Int, origEdge: Int, targets: Seq[Int], reach: Int): Int =
    val pos = origEdge + delta
    delta + targets.iterator.map(t => t - pos).filter(f => math.abs(f) <= reach).minByOption(math.abs).getOrElse(0)

  /** The id of the clip block under widget-local x `px`, if any — how a press on a media lane picks a
    * placed clip to select or drag. Clips on a track never overlap, so the first hit wins. */
  def clipAt(px: Double, view: View, blocks: Seq[ClipBlock]): Option[String] =
    blocks.find(b => px >= view.xOf(b.start) && px <= view.xOf(b.start + b.length)).map(_.id)

  /** The lowest and highest timeline start a clip of `length` at `origStart` may take while staying in
    * its current gap between neighbours (given as (start, length) pairs, excluding this clip) and within
    * `[0, total)`. Confining a dragged clip to its gap keeps a track's clips a non-overlapping sequence,
    * which is what the graph's per-track playlist requires. */
  def clipStartBounds(origStart: Int, length: Int, total: Int, others: Seq[(Int, Int)]): (Int, Int) =
    val origEnd   = origStart + length
    val prevEnd   = others.collect { case (s, l) if s + l <= origStart => s + l }.maxOption.getOrElse(0)
    val nextStart = others.collect { case (s, _) if s >= origEnd => s }.minOption.getOrElse(total)
    (math.max(0, prevEnd), math.max(0, math.min(total, nextStart) - length))

  /** The trim edge under widget-local x `px`, if the cursor is within a few pixels of a block's left or
    * right edge — how a press on a media lane picks a clip edge to trim rather than the body to move. The
    * left edge wins over the right when both are near (a very short block), and a hit is preferred to the
    * whole-body `clipAt` in the caller so the edges stay grabbable. */
  def clipEdgeAt(px: Double, view: View, blocks: Seq[ClipBlock]): Option[(String, TrimEdge)] =
    val band = 6.0
    blocks.iterator.flatMap { b =>
      val x1 = view.xOf(b.start)
      val x2 = view.xOf(b.start + b.length)
      if math.abs(px - x1) <= band then Some((b.id, TrimEdge.Left))
      else if math.abs(px - x2) <= band then Some((b.id, TrimEdge.Right))
      else None
    }.nextOption()

  /** The range of frame deltas a trim of `edge` may apply, staying on the source and in the clip's gap.
    * A placement plays `length` frames from `inPoint` into a source `srcLen` frames long, starting at
    * `start`; `others` are the same-track neighbours (start, length) it must not overlap.
    *
    *   - **Right** edge (the out-point): a delta grows (`+`) or shrinks (`-`) the length, kept `>= 1`
    *     frame, never past the source's end (`inPoint + length <= srcLen`) and never over the next clip.
    *   - **Left** edge (the in-point): a delta moves the in-point and the start together — `+` trims the
    *     head (revealing later source, a shorter clip), `-` reveals earlier source — kept so `inPoint`
    *     stays `>= 0`, the start stays past the previous clip, and the length stays `>= 1`.
    *
    * A linked pair intersects the ranges of its halves so both trim by one delta and stay locked. */
  def clipTrimBounds(
      edge: TrimEdge, start: Int, inPoint: Int, length: Int, srcLen: Int, total: Int, others: Seq[(Int, Int)],
  ): (Int, Int) =
    val end       = start + length
    val prevEnd   = others.collect { case (s, l) if s + l <= start => s + l }.maxOption.getOrElse(0)
    val nextStart = others.collect { case (s, _) if s >= end => s }.minOption.getOrElse(total)
    edge match
      case TrimEdge.Right =>
        val lo = 1 - length
        val hi = math.min(srcLen - inPoint - length, nextStart - end)
        (lo, hi)
      case TrimEdge.Left =>
        val lo = math.max(-inPoint, prevEnd - start)
        val hi = length - 1
        (lo, hi)

  /** Where a new placement of a clip `srcLen` frames long should land when dropped at `atFrame`, given
    * the existing clips on the two tracks it occupies — `aBlocks` and `bBlocks` as (start, length) pairs
    * (an unlinked clip passes its one track's blocks and `Nil` for the other). The drop point is snapped
    * forward past any clip it falls inside on either track (repeatedly, since clearing one track's clip
    * can land inside another's), then the length is trimmed to fit the gap before the next clip on either
    * track and never to run past the source's own length. Returns (start, length); a length of 0 or less
    * means the drop point has no room. Placing a linked A/V pair uses both tracks so picture and sound
    * land at one start and one length; a single clip uses one. */
  def freePlacement(atFrame: Int, srcLen: Int, aBlocks: Seq[(Int, Int)], bBlocks: Seq[(Int, Int)]): (Int, Int) =
    val all   = aBlocks ++ bBlocks
    var start = math.max(0, atFrame)
    var moved = true
    while moved do
      moved = false
      for (s, l) <- all do
        if start >= s && start < s + l then
          start = s + l
          moved = true
    val nextA  = aBlocks.collect { case (s, _) if s >= start => s }.minOption.getOrElse(Int.MaxValue)
    val nextB  = bBlocks.collect { case (s, _) if s >= start => s }.minOption.getOrElse(Int.MaxValue)
    val room   = math.min(nextA, nextB) - start
    (start, math.min(srcLen, room))

  /** The project after dropping the payload `payloadId` — a bin clip *or* a lower third (title) — onto
    * the track `trackId` at timeline frame `frame`, `srcLen` frames available (a clip's source length, or
    * the default length to give a title, which has no intrinsic length). This is what a drag from the bin
    * resolves to.
    *
    *   - a **video** clip dropped on a **video** track lands as a linked A/V pair — its picture on that
    *     track and its sound on the audio track sharing that video track's number (created if absent), at
    *     one start both can hold (so they read and move as one);
    *   - an **audio** clip on an **audio** track lands there;
    *   - a **title** on a **video** track lands as a `titleId` placement, drawing its card over the tracks
    *     below.
    *
    * Any other pairing (an audio clip on a video track, a title on an audio track), or a drop with no
    * room, returns the project unchanged (the same reference), so a caller can tell nothing happened. The
    * landing point and length come from [[freePlacement]], so a drop into a gap fills it and a drop onto a
    * clip snaps past it — the same math the drop-preview ghost uses. */
  def dropClip(project: Project, payloadId: String, trackId: String, frame: Int, srcLen: Int): Project =
    def blocksOf(clips: List[PlacedClip]): Seq[(Int, Int)] = clips.map(c => (c.timelineStart, c.length))
    project.tracks.find(_.id == trackId) match
      case None => project
      case Some(pt) => project.titleFor(payloadId) match
        // A title placed on a video track: a `titleId` placement across a default-length window, snapped
        // into a gap like any clip. A title dropped on an audio track is rejected.
        case Some(_) =>
          if pt.kind != MediaKind.Video then project
          else
            val (start, length) = freePlacement(frame, srcLen, blocksOf(pt.clips), Nil)
            if length <= 0 then project
            else project.updateTrack(pt.id)(x => x.copy(clips = x.clips :+ PlacedClip.makeTitle(payloadId, start, length)))
        case None => project.clipFor(payloadId) match
          case None => project
          case Some(clip) =>
            // `pt`/`at` are project tracks; land one placement on a single track (audio here).
            def onOne(track: Project => (String, List[PlacedClip])): Project =
              val (tid, clips)    = track(project)
              val (start, length) = freePlacement(frame, srcLen, blocksOf(clips), Nil)
              if length <= 0 then project
              else project.updateTrack(tid)(x => x.copy(clips = x.clips :+ PlacedClip.make(payloadId, start, length)))
            (clip.kind, pt.kind) match
              case (MediaKind.Video, MediaKind.Video) =>
                // Pair the picture with the audio track that shares this video track's number — V2's sound
                // goes to A2, V3's to A3 — created if it does not exist yet, so each camera's sound lands on
                // its own lane, aligned under its picture. (Always sending it to A1 was the bug: a second
                // camera on V2 collided with the first camera's audio there, and when A1 was full at the
                // drop point the placement had no room and silently did nothing.)
                val (withAt, at) = pt.num.flatMap(n => project.audioTracks.find(_.num.contains(n))) match
                  case Some(a) => (project, a)
                  case None =>
                    val an = pt.num.getOrElse(project.tracks.flatMap(_.num).maxOption.getOrElse(0) + 1)
                    // The project `Track`, fully qualified — inside `object Timeline` the bare name is the
                    // nested block model, not the project track.
                    val na = io.github.edadma.kutter.Track(s"trk-${System.nanoTime()}", s"A$an", MediaKind.Audio)
                    (project.copy(tracks = project.tracks :+ na), na)
                val (start, length) = freePlacement(frame, srcLen, blocksOf(pt.clips), blocksOf(at.clips))
                if length <= 0 then project
                else
                  val link = Some(s"lnk-${System.nanoTime()}")
                  withAt.copy(tracks = withAt.tracks.map(t =>
                    if t.id == pt.id || t.id == at.id then t.copy(clips = t.clips :+ PlacedClip.make(payloadId, start, length, link = link))
                    else t))
              case (MediaKind.Audio, MediaKind.Audio) => onOne(_ => (pt.id, pt.clips))
              case _                                  => project // an incompatible clip/track pairing: no drop

  private def playheadInk(theme: Theme): Color = theme.accent

  /** Paint the ruler widget: a faint band with a tick and a centred m:ss label at a spacing that
    * stays uncrowded at the view's scale, covering only the seconds the view shows, plus the
    * playhead's grip and top line. Frames are converted to seconds at `fps` — the timeline's own
    * frame rate — so a differently-rated project labels time correctly (a 24 fps clip ends at its
    * real second, not compressed by a fixed 30). */
  def paintRuler(cv: Canvas, size: Size, view: View, position: Int, fps: Double, theme: Theme): Unit =
    val w       = size.width
    val rulerBg = if theme.isDark then Color.rgb(0x22262a) else Color.rgb(0xd0d4d8)
    cv.fillRect(Rect(0, 0, w, size.height), rulerBg)

    val minTickPx = 56.0
    val pxPerSec  = fps * view.pxPerFrame
    val step      = math.max(1, math.ceil(minTickPx / math.max(1e-9, pxPerSec)).toInt)
    val tickInk    = theme.border
    val labelInk   = if theme.isDark then Color.rgb(0x8b9096) else Color.rgb(0x60656a)
    val labelStyle = TextStyle(11.0, labelInk)
    val firstSec   = math.max(0, (math.floor(view.frameAtPx(0) / fps).toInt / step) * step)
    val lastSec    = math.ceil(view.frameAtPx(w) / fps).toInt
    var s          = firstSec
    while s <= lastSec do
      val x     = view.xOf(s * fps)
      val label = f"${s / 60}:${s % 60}%02d"
      val lw    = cv.measureText(label, labelStyle).width
      cv.line(Offset(x, size.height - 6), Offset(x, size.height), 1.0, tickInk)
      cv.drawText(Offset(x - lw / 2, 3), label, labelStyle)
      s += step

    val px   = view.xOf(position)
    val head = playheadInk(theme)
    cv.fillRect(Rect(px - 0.5, 8, 1.5, size.height - 8), head)
    cv.fillPath(Path.polyline(Seq(Offset(px - 5, 0), Offset(px + 5, 0), Offset(px, 8))), head)

  /** Paint one track widget: each of its placed clips as a block at its timeline position — a filmstrip
    * where the strip is ready and a waveform for an audio clip, flat otherwise — with a bright top cap,
    * a border (ringed when selected), a link pip on a linked half, and the clip's name; then the
    * playhead line over them all. */
  def paintTrack(cv: Canvas, size: Size, track: Track, view: View, position: Int, theme: Theme): Unit =
    val w  = size.width
    val h  = size.height
    val bh = h - 4 // the clip block's height, inset so stacked tracks read as separate lanes

    val laneBg   = if theme.isDark then Color.rgb(0x16191c) else Color.rgb(0xe3e6e9)
    val blockCol = if theme.isDark then Color.rgb(0x2f5d8a) else Color.rgb(0xa5c8ec)
    val blockTop = theme.primary
    val radius   = BorderRadius.all(6)
    cv.fillRect(Rect(0, 0, w, h), laneBg)

    val audioBg = if theme.isDark then Color.rgb(0x24303a) else Color.rgb(0xd4e2ee)
    val waveInk = if theme.isDark then Color.rgb(0x74c0fc) else Color.rgb(0x3b7bb8)

    // Each placed clip is a block from its start for its length. Blocks never overlap, so they paint
    // independently, left to right along the lane; a block wholly outside the view is skipped.
    for b <- track.clips do
      val x1   = view.xOf(b.start)
      val x2   = view.xOf(b.start + b.length)
      if x2 >= 0 && x1 <= w then
        paintClipBlock(cv, b, x1, x2, bh, w, blockCol, blockTop, audioBg, waveInk, radius, theme)

    val px = view.xOf(position)
    cv.fillRect(Rect(px - 0.5, 0, 1.5, h), playheadInk(theme))

  /** Paint one clip block spanning `x1..x2` (already view-mapped; possibly partly off-screen) on a
    * media lane: its filmstrip or waveform, cap, label scrim, link pip, and border. */
  private def paintClipBlock(
      cv: Canvas, b: ClipBlock, x1: Double, x2: Double, bh: Double, w: Double,
      blockCol: Color, blockTop: Color, audioBg: Color, waveInk: Color, radius: BorderRadius, theme: Theme,
  ): Unit =
      val bw   = math.max(2.0, x2 - x1 - 2)
      val span = math.max(1.0, x2 - x1)
      val r    = Rect(x1 + 1, 2, bw, bh)

      // A block fraction (0..1 across the block) maps to a fraction of the *whole source* the generators
      // span, so a trimmed block shows exactly the slice it plays. With no measured source length, fall
      // back to a one-to-one mapping (the block is the whole source, as before trimming existed).
      def srcFrac(blockFrac: Double): Double =
        if b.srcLen > 0 then (b.inPoint + blockFrac * b.length) / b.srcLen else blockFrac

      b.titleColor match
        // A lower-third title placed on a video track: a flat block in the title's tint (no filmstrip or
        // waveform — a still card has neither), with the same bright cap the video clips carry.
        case tint: Color =>
          cv.fillRoundedRect(r, radius, tint)
          cv.fillRoundedRect(Rect(x1 + 1, 2, bw, 4), BorderRadius.all(2), blockTop)
        case null => b.waveform match
          case wf: Waveform =>
            // An audio clip: the peak envelope mirrored around the block's midline.
            cv.fillRoundedRect(r, radius, audioBg)
            cv.pushClip(r, radius)
            val mid   = 2 + bh / 2
            val halfH = (bh / 2) * 0.88
            cv.fillRect(Rect(x1 + 1, mid - 0.25, bw, 0.5), waveInk) // centre line
            // Only the visible slice of the envelope is sampled — a long clip mostly off-screen
            // costs what its on-screen pixels cost, not its full width.
            var sx = math.max(x1 + 1, 0.0)
            val ex = math.min(x2 - 1, w)
            while sx < ex do
              val hh = wf.at(srcFrac((sx - x1) / span)) * halfH
              if hh > 0.3 then cv.fillRect(Rect(sx, mid - hh, 1.3, hh * 2), waveInk)
              sx += 1.6
            cv.popClip()

          case null =>
            // A video clip: a filmstrip where the strip is ready, flat otherwise, with a bright cap.
            b.thumbs match
              case t: Thumbnails =>
                cv.fillRoundedRect(r, radius, blockCol)
                cv.pushClip(r, radius)
                val tw = math.max(8.0, bh * 16.0 / 9.0)
                // Start at the first tile that reaches the view and stop past its right edge, so an
                // off-screen stretch of filmstrip draws nothing.
                var sx = x1 + 1 + math.max(0.0, math.floor((-(x1 + 1) - tw) / tw) + 1) * tw
                val ex = math.min(x2 - 1, w + tw)
                while sx < ex do
                  t.at(srcFrac((sx - x1) / span)) match
                    case img: RasterImage => cv.drawImage(img, Rect(sx, 2, tw, bh))
                    case null             => ()
                  sx += tw
                cv.popClip()
              case null =>
                cv.fillRoundedRect(r, radius, blockCol)
            cv.fillRoundedRect(Rect(x1 + 1, 2, bw, 4), BorderRadius.all(2), blockTop)

      // The clip's name, over a soft scrim so it reads on either a filmstrip or a waveform.
      val label = b.label
      val ls    = TextStyle(11.0, Color.white)
      val lw    = cv.measureText(label, ls).width
      if bw > 24 then
        cv.pushClip(r, radius)
        cv.fillRect(Rect(x1 + 1, 2, math.min(bw, lw + 12), 16), Color(0, 0, 0, 115))
        cv.drawText(Offset(x1 + 7, 4), label, ls)
        cv.popClip()

      // A link pip marks a clip that moves with its A/V partner (the picture and its sound stay locked).
      if b.linked then cv.fillRoundedRect(Rect(x2 - 9, 4, 5, 5), BorderRadius.all(2.5), theme.accent)

      if b.selected then cv.strokeRoundedRect(r, radius, theme.primary, 2.0)
      else cv.strokeRoundedRect(r, radius, theme.border, 1.0)
