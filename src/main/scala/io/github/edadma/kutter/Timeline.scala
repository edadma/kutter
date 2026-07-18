package io.github.edadma.kutter

import io.github.edadma.suit.*

// The timeline: the editor's spine. It is not one widget but several — a pinned time ruler across the
// top and one widget per track below it — composed in `Main` into the track panel, with the tracks in
// a scroll view so a tall stack scrolls while the ruler stays put. This object holds the shared
// geometry and the per-widget painters; each track paints its own clips and its own segment of the
// playhead, and because every widget shares the same width and time mapping the segments line up into
// one continuous playhead. The base video track plus, in time, the overlay tracks the lower thirds
// ride on are just more entries in that stack.
object Timeline:

  val RulerHeight = 22.0 // the time-ruler widget's height
  val TrackHeight = 68.0 // one track widget's height
  val LabelWidth  = 60.0 // the track-name column down the left, as in any editor; the ruler leaves it blank

  /** One lower third's block on the titles lane: its window on the timeline, a caption, the colour it
    * wears (drawn from its style), and whether it is the selected one. */
  case class OverlayBlock(
      id:       String,
      inFrame:  Int,
      outFrame: Int,
      label:    String,
      color:    Color,
      selected: Boolean,
  )

  /** One placed clip's block on a media lane: where it sits on the timeline (`start` for `length`
    * frames), a caption, whether it is one half of a linked A/V pair, whether it is the selected clip,
    * and the generator that fills it — a filmstrip (`thumbs`, on a video track) or a waveform overview
    * (`waveform`, on an audio track). A block carries whichever one its track draws. The generator is
    * keyed by source, so several placements of the same clip share one; the block's geometry is the
    * live placement, so a clip drawn here always sits where the project puts it. */
  case class ClipBlock(
      id:       String,
      start:    Int,
      length:   Int,
      label:    String,
      linked:   Boolean,
      selected: Boolean,
      thumbs:   Thumbnails | Null = null,
      waveform: Waveform | Null   = null,
  )

  /** One track: a named lane and what rides on it. A media track sequences its placed `clips` at their
    * timeline positions (each a filmstrip or a waveform); the titles lane carries a set of lower-third
    * `overlays` instead. A track sets whichever one it draws. */
  case class Track(
      name:     String,
      clips:    Seq[ClipBlock]          = Nil,
      overlays: Seq[OverlayBlock] | Null = null,
  )

  /** The frame-count-to-x mapping for a widget `width` wide holding `total` frames. */
  private def xOf(frame: Double, total: Int, width: Double): Double =
    (frame / math.max(1, total)) * width

  /** The inverse: the frame under widget-local x `px`, clamped. Turns a cursor position into a seek. */
  def frameAt(px: Double, total: Int, width: Double): Int =
    val f = math.round((px / math.max(1.0, width)) * total).toInt
    math.max(0, math.min(total - 1, f))

  /** Where a dragged title block's in-frame lands: its `origIn` shifted by how far the cursor has
    * moved from where it grabbed (`curFrame - grabFrame`), then clamped so the whole block (length
    * `len`) stays within `[0, total)`. Keeps the block's length and never lets it run off either end. */
  def dragPlacement(origIn: Int, len: Int, grabFrame: Int, curFrame: Int, total: Int): Int =
    math.max(0, math.min(total - 1 - len, origIn + (curFrame - grabFrame)))

  /** The id of the topmost overlay block under widget-local x `px`, if any — how a click on the titles
    * lane picks a lower third. Blocks are tested back-to-front so the one drawn on top wins when two
    * windows overlap. */
  def overlayAt(px: Double, total: Int, width: Double, blocks: Seq[OverlayBlock]): Option[String] =
    blocks.reverseIterator
      .find(b => px >= xOf(b.inFrame, total, width) && px <= xOf(b.outFrame, total, width))
      .map(_.id)

  /** The id of the clip block under widget-local x `px`, if any — how a press on a media lane picks a
    * placed clip to select or drag. Clips on a track never overlap, so the first hit wins. */
  def clipAt(px: Double, total: Int, width: Double, blocks: Seq[ClipBlock]): Option[String] =
    blocks.find(b => px >= xOf(b.start, total, width) && px <= xOf(b.start + b.length, total, width)).map(_.id)

  /** The lowest and highest timeline start a clip of `length` at `origStart` may take while staying in
    * its current gap between neighbours (given as (start, length) pairs, excluding this clip) and within
    * `[0, total)`. Confining a dragged clip to its gap keeps a track's clips a non-overlapping sequence,
    * which is what the graph's per-track playlist requires. */
  def clipStartBounds(origStart: Int, length: Int, total: Int, others: Seq[(Int, Int)]): (Int, Int) =
    val origEnd   = origStart + length
    val prevEnd   = others.collect { case (s, l) if s + l <= origStart => s + l }.maxOption.getOrElse(0)
    val nextStart = others.collect { case (s, _) if s >= origEnd => s }.minOption.getOrElse(total)
    (math.max(0, prevEnd), math.max(0, math.min(total, nextStart) - length))

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

  private def playheadInk(theme: Theme): Color = theme.accent

  /** A readable ink for a caption over `bg`: black on a light block, white on a dark one. */
  private def readableInk(bg: Color): Color =
    if 0.299 * bg.r + 0.587 * bg.g + 0.114 * bg.b > 150 then Color.black else Color.white

  /** Paint the ruler widget: a faint band with a tick and a centred m:ss label at a spacing that
    * stays uncrowded on a short window, plus the playhead's grip and top line. */
  def paintRuler(cv: Canvas, size: Size, total: Int, position: Int, theme: Theme): Unit =
    val w       = size.width
    val rulerBg = if theme.isDark then Color.rgb(0x22262a) else Color.rgb(0xd0d4d8)
    cv.fillRect(Rect(0, 0, w, size.height), rulerBg)

    val fps        = 30.0
    val totalSecs  = total / fps
    val minTickPx  = 56.0
    val secPerPx   = totalSecs / math.max(1.0, w)
    val step       = math.max(1, math.ceil(minTickPx * secPerPx).toInt)
    val tickInk    = theme.border
    val labelInk   = if theme.isDark then Color.rgb(0x8b9096) else Color.rgb(0x60656a)
    val labelStyle = TextStyle(11.0, labelInk)
    var s          = 0
    while s <= totalSecs.toInt do
      val x     = xOf(s * fps, total, w)
      val label = f"${s / 60}:${s % 60}%02d"
      val lw    = cv.measureText(label, labelStyle).width
      val tx    = math.max(1.0, math.min(w - lw - 1.0, x - lw / 2))
      cv.line(Offset(x, size.height - 6), Offset(x, size.height), 1.0, tickInk)
      cv.drawText(Offset(tx, 3), label, labelStyle)
      s += step

    val px   = xOf(position, total, w)
    val head = playheadInk(theme)
    cv.fillRect(Rect(px - 0.5, 8, 1.5, size.height - 8), head)
    cv.fillPath(Path.polyline(Seq(Offset(px - 5, 0), Offset(px + 5, 0), Offset(px, 8))), head)

  /** Paint one track widget: each of its placed clips as a block at its timeline position — a filmstrip
    * where the strip is ready and a waveform for an audio clip, flat otherwise — with a bright top cap,
    * a border (ringed when selected), a link pip on a linked half, and the clip's name; then the
    * playhead line over them all. */
  def paintTrack(cv: Canvas, size: Size, track: Track, total: Int, position: Int, theme: Theme): Unit =
    val w  = size.width
    val h  = size.height
    val bh = h - 4 // the clip block's height, inset so stacked tracks read as separate lanes

    val laneBg   = if theme.isDark then Color.rgb(0x16191c) else Color.rgb(0xe3e6e9)
    val blockCol = if theme.isDark then Color.rgb(0x2f5d8a) else Color.rgb(0xa5c8ec)
    val blockTop = theme.primary
    val radius   = BorderRadius.all(6)
    cv.fillRect(Rect(0, 0, w, h), laneBg)

    // The titles lane paints lower-third blocks instead of clips and returns early.
    val overlays = track.overlays
    if overlays != null then
      paintOverlays(cv, overlays, total, w, bh, theme)
      val hx = xOf(position, total, w)
      cv.fillRect(Rect(hx - 0.5, 0, 1.5, h), playheadInk(theme))
      return

    val audioBg = if theme.isDark then Color.rgb(0x24303a) else Color.rgb(0xd4e2ee)
    val waveInk = if theme.isDark then Color.rgb(0x74c0fc) else Color.rgb(0x3b7bb8)

    // Each placed clip is a block from its start for its length. Blocks never overlap, so they paint
    // independently, left to right along the lane.
    for b <- track.clips do
      val x1   = xOf(b.start, total, w)
      val x2   = xOf(b.start + b.length, total, w)
      val bw   = math.max(2.0, x2 - x1 - 2)
      val span = math.max(1.0, x2 - x1)
      val r    = Rect(x1 + 1, 2, bw, bh)

      b.waveform match
        case wf: Waveform =>
          // An audio clip: the peak envelope mirrored around the block's midline.
          cv.fillRoundedRect(r, radius, audioBg)
          cv.pushClip(r, radius)
          val mid   = 2 + bh / 2
          val halfH = (bh / 2) * 0.88
          cv.fillRect(Rect(x1 + 1, mid - 0.25, bw, 0.5), waveInk) // centre line
          var sx = x1 + 1
          while sx < x2 - 1 do
            val hh = wf.at((sx - x1) / span) * halfH
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
              var sx = x1 + 1
              while sx < x2 - 1 do
                t.at((sx - x1) / span) match
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

    val px = xOf(position, total, w)
    cv.fillRect(Rect(px - 0.5, 0, 1.5, h), playheadInk(theme))

  /** Paint the titles lane: each lower third as a rounded block across its in/out window, filled with
    * its style's colour and captioned with its name. The selected block is ringed in the primary
    * colour so it reads as the inspector's subject; the others carry a plain border. */
  private def paintOverlays(
      cv: Canvas, blocks: Seq[OverlayBlock], total: Int, w: Double, bh: Double, theme: Theme,
  ): Unit =
    val radius = BorderRadius.all(6)
    for b <- blocks do
      val x1 = xOf(b.inFrame, total, w)
      val x2 = xOf(b.outFrame, total, w)
      val bw = math.max(2.0, x2 - x1 - 2)
      val r  = Rect(x1 + 1, 2, bw, bh)
      cv.fillRoundedRect(r, radius, b.color)

      val ink   = readableInk(b.color)
      val style = TextStyle(11.0, ink)
      cv.pushClip(r, radius)
      val pad   = 6.0
      val label = b.label
      val lw    = cv.measureText(label, style).width
      if lw <= bw - pad * 2 || bw > 40 then
        cv.drawText(Offset(x1 + 1 + pad, 2 + (bh - 11) / 2), label, style)
      cv.popClip()

      if b.selected then cv.strokeRoundedRect(r, radius, theme.primary, 2.0)
      else cv.strokeRoundedRect(r, radius, theme.border, 1.0)
