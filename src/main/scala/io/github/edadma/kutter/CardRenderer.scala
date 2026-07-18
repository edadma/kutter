package io.github.edadma.kutter

import java.io.File

import io.github.edadma.libcairo.{Context, Format, FontSlant, FontWeight, imageSurfaceCreate}

// Draws a lower third's card — a full-frame transparent image with the overlay's words dressed in a
// chosen `Style`, composited onto the base video on its own track (see `Player`). The style holds
// everything about the look (bar, stripe, colours, fonts, placement); this renderer holds only how
// to draw it, so a new look is a new `Style`, not new code here.
//
// Two renderers meet behind `renderCard`: a texish document is typeset by `TexishCard` (real kerning,
// chosen fonts), and the hand-rolled Cairo text is drawn here. Which one draws is a three-way choice —
// the card's own `body` if it has one, else the style's texish template, else the Cairo look — but all
// three hand back the same thing, a full-frame transparent PNG, so the compositing path is identical.
object CardRenderer:

  /** Render `lt` in `style` to a full-frame transparent PNG sized to the profile, and return its
    * path. The file is cached by the content, style, and size, so the same card is drawn once however
    * many times it is placed. The card's own `body` wins; failing that a style's texish template is
    * typeset by `TexishCard`; failing that the Cairo renderer below draws it. */
  def renderCard(lt: LowerThird, style: Style, width: Int, height: Int): String =
    val file = cacheFile(lt, style, width, height)
    if !file.exists() then
      file.getParentFile.mkdirs()
      lt.body.orElse(style.texish) match
        case Some(document) => TexishCard.render(document, lt.name, lt.title, width, height, file.getAbsolutePath)
        case None           => draw(lt, style, width, height, file.getAbsolutePath)
    file.getAbsolutePath

  /** Draw the card onto a fresh ARGB surface (which starts fully transparent) and write it as a PNG.
    * The bar is placed along the bottom by the style's anchor and sized by its fractions; the name
    * sits in bold over a lighter title, each aligned to the anchor, with an optional soft shadow for
    * bar-less styles. */
  private def draw(lt: LowerThird, style: Style, width: Int, height: Int, path: String): Unit =
    val surface = imageSurfaceCreate(Format.ARGB32, width, height)
    val cr      = surface.create

    val barW = width * style.widthFrac
    val barH = height * style.heightFrac
    val barY = height * 0.74
    val barX = style.anchor match
      case Anchor.LowerLeft   => width * 0.06
      case Anchor.LowerCenter => (width - barW) / 2
      case Anchor.LowerRight  => width * 0.94 - barW
    val pad = barH * 0.20

    // The bar, drawn only when the style asks for one; a fully transparent bar is a bar-less style.
    if style.bar.a > 0 then
      val radius = barH * style.cornerRadius
      roundedRect(cr, barX, barY, barW, barH, radius)
      setColor(cr, style.bar)
      cr.fill()

    // The accent stripe, flush with the bar's leading edge, when the style carries one.
    val stripeW = style.stripe match
      case Some(color) if style.bar.a > 0 =>
        val w      = barH * 0.10
        val radius = barH * style.cornerRadius
        roundedRect(cr, barX, barY, w, barH, radius)
        setColor(cr, color)
        cr.fill()
        w
      case _ => 0.0

    // The text block's left edge (past the stripe); each line is then aligned to the style's anchor.
    val leftEdge = barX + stripeW + pad
    drawLine(cr, lt.name, style, style.nameColor, bold = style.nameBold,
      size = barH * 0.34, baseline = barY + barH * 0.42, barX, barW, leftEdge, pad)
    drawLine(cr, lt.title, style, style.titleColor, bold = false,
      size = barH * 0.22, baseline = barY + barH * 0.74, barX, barW, leftEdge, pad)

    cr.destroy()
    surface.flush()
    surface.writeToPNG(path)
    surface.destroy()

  /** Draw one line of text at `size`, aligned to the style's anchor within the bar, optionally with
    * a soft drop shadow so a bar-less style stays legible over bright footage. */
  private def drawLine(
      cr: Context, text: String, style: Style, color: Rgba, bold: Boolean,
      size: Double, baseline: Double, barX: Double, barW: Double, leftEdge: Double, pad: Double,
  ): Unit =
    cr.selectFontFace("sans-serif", FontSlant.NORMAL, if bold then FontWeight.BOLD else FontWeight.NORMAL)
    cr.setFontSize(size)
    val tw = cr.textExtents(text).width
    val x = style.anchor match
      case Anchor.LowerLeft   => leftEdge
      case Anchor.LowerCenter => barX + (barW - tw) / 2
      case Anchor.LowerRight  => barX + barW - pad - tw

    if style.shadow then
      cr.moveTo(x + size * 0.04, baseline + size * 0.04)
      cr.setSourceRGBA(0, 0, 0, 0.6)
      cr.showText(text)

    cr.moveTo(x, baseline)
    setColor(cr, color)
    cr.showText(text)

  private def setColor(cr: Context, c: Rgba): Unit = cr.setSourceRGBA(c.r, c.g, c.b, c.a)

  /** A rounded-rectangle path built from four quarter-circle arcs. */
  private def roundedRect(cr: Context, x: Double, y: Double, w: Double, h: Double, r: Double): Unit =
    val deg = math.Pi / 180.0
    cr.newSubPath()
    cr.arc(x + w - r, y + r, r, -90 * deg, 0 * deg)
    cr.arc(x + w - r, y + h - r, r, 0 * deg, 90 * deg)
    cr.arc(x + r, y + h - r, r, 90 * deg, 180 * deg)
    cr.arc(x + r, y + r, r, 180 * deg, 270 * deg)
    cr.closePath()

  private def cacheFile(lt: LowerThird, style: Style, width: Int, height: Int): File =
    // The card's own body and the style's template are part of the key so editing either redraws the
    // card rather than serving the stale cached picture; a Cairo style contributes only its id (its
    // look is fixed).
    val content = s"${lt.name} ${lt.title} ${style.id} ${style.texish.getOrElse("")} ${lt.body.getOrElse("")}"
    val key     = f"${content.hashCode & 0xffffffffL}%08x-${width}x$height.png"
    new File(new File(System.getProperty("user.home"), ".cache/kutter/lowerthirds"), key)
