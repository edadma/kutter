package io.github.edadma.kutter

import io.github.edadma.sdl3.*
import io.github.edadma.libcairo.{Format, FontSlant, FontWeight, imageSurfaceCreate, imageSurfaceCreateFromPNG}

// A startup splash in its own small, borderless, centred window: the logo composited onto a dark
// field and shown for a beat before the main player window opens. It runs before `Suit.run`, on the
// main thread, talking to SDL directly (kutter's only direct SDL windowing) and building the image
// with Cairo. Purely cosmetic — any failure is swallowed so a missing splash never blocks startup.
object Splash:

  /** The app's version, a calendar date (YYYY.MM.DD) — shown on the splash as a product's build tag is.
    * Bump it when cutting a release. */
  val Version = "2026.07.18"

  /** Show the logo splash for `durationMs`, then tear it down and return. */
  def show(logoPath: String, durationMs: Long): Unit =
    if !init(INIT_VIDEO) then return

    val w = 460
    val h = 200

    // Compose in Cairo: fill dark (matching the player window's clear colour), then paint the logo
    // scaled to fit and centred. The result is opaque, so the SDL texture needs no alpha blending.
    val surf = imageSurfaceCreate(Format.ARGB32, w, h)
    val cr   = surf.create
    cr.setSourceRGB(24 / 255.0, 24 / 255.0, 28 / 255.0)
    cr.paint()

    val logo = imageSurfaceCreateFromPNG(logoPath)
    val lw   = logo.getWidth.toDouble
    val lh   = logo.getHeight.toDouble
    if lw > 0 && lh > 0 then
      val scale = math.min((w - 48) / lw, (h - 48) / lh)
      cr.save()
      cr.translate((w - lw * scale) / 2, (h - lh * scale) / 2)
      cr.scale(scale, scale)
      cr.setSourceSurface(logo, 0, 0)
      cr.paint()
      cr.restore()

    // The version, a date, muted in the lower-right — a product splash's build tag.
    cr.selectFontFace("sans-serif", FontSlant.NORMAL, FontWeight.NORMAL)
    cr.setFontSize(13)
    cr.setSourceRGBA(1, 1, 1, 0.5)
    val vext = cr.textExtents(Version)
    cr.moveTo(w - vext.width - 16, h - 14)
    cr.showText(Version)
    surf.flush()

    val win = createWindow("kutter", w, h, WINDOW_BORDERLESS)
    displayUsableBounds(getPrimaryDisplay).foreach { (ux, uy, uw, uh) =>
      win.setPosition(ux + (uw - w) / 2, uy + (uh - h) / 2)
    }
    val ren = win.createRenderer()
    val tex = ren.createTexture(PIXELFORMAT_ARGB8888, TEXTUREACCESS_STREAMING, w, h)
    tex.update(surf.getData, surf.getStride)

    // Keep the window painted and the OS happy (drain events, or macOS marks it unresponsive) for
    // the duration.
    val start = System.nanoTime()
    while (System.nanoTime() - start) / 1000000L < durationMs do
      var e = pollEvent()
      while e.isDefined do e = pollEvent()
      ren.copy(tex)
      ren.present()
      Thread.sleep(16)

    tex.destroy()
    ren.destroy()
    win.destroy()
    cr.destroy()
    surf.destroy()
    logo.destroy()
