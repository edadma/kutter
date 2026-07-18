package io.github.edadma.kutter

import java.io.File

import io.github.edadma.libcairo.{Format, Surface, imageSurfaceCreate}
import io.github.edadma.texish.{CairoImageTypesetter, CairoTypesetter, Color, Passes, Typesetter}
import io.github.edadma.texish.parser.{Processor, TypesetterHandler, registerTypesettingPrimitives}

// Renders a lower third's card with texish, the alternate to `CardRenderer`'s hand-drawn Cairo look.
// A card is a full-frame transparent image the compositor lays over the video (see `Player`), so this
// produces exactly what `CardRenderer` does — a transparent PNG sized to the frame — and the two are
// interchangeable behind `CardRenderer.renderCard`.
//
// The card's words are not spliced into the document text; they are handed to the engine as the
// `ltname` and `lttitle` variables, which the template reads with `\the\ltname` / `\the\lttitle`. The
// engine emits a variable's value as literal text, so a name carrying a backslash or a brace is set as
// written rather than parsed as texish — no escaping, and no template can be broken by the data.
//
// The page is run at 72 dpi so one of the engine's points is one device pixel: a template written for
// a 1280×720 card thinks in the same numbers the frame is measured in. The page background is
// transparent (not the usual paper white), so only where the template draws does the card cover the
// video beneath it.
object TexishCard:

  /** Render `template` to a full-frame transparent PNG at `path`, with the overlay's `name` and
    * `title` available to the document as the `ltname` and `lttitle` variables. */
  def render(template: String, name: String, title: String, width: Int, height: Int, path: String): Unit =
    ensureFonts()
    val t = Passes.untilStable() { () =>
      val ts = new CairoImageTypesetter(72.0) // 72 dpi ⇒ one engine point maps to one device pixel
      ts.backgroundColor = Color.TRANSPARENT
      ts.set("paperwidth", width.toDouble)
      ts.set("paperheight", height.toDouble)
      ts.set("ltname", name)
      ts.set("lttitle", title)
      ts
    }(typeset(_, template))

    // Each shipped page is its own ARGB32 surface and belongs to us once handed out; write the first
    // (a card is one page) then free them all. A template that ships nothing leaves a fully transparent
    // card, so the compositing path still finds a real file to place.
    val pages = t.getDocument.printedPages
    pages.headOption match
      case Some(s: Surface) =>
        s.flush()
        s.writeToPNG(path)
      case _ => writeTransparent(width, height, path)
    pages.foreach {
      case s: Surface => s.destroy()
      case _          =>
    }
    t.destroy()

  private var fontsConfigured = false

  // Point texish at its bundled font files. The engine names each face `fonts/…` and reads it under
  // `CairoTypesetter.fontsDir`, so that directory is the one *containing* the `fonts/` folder — which is
  // nothing kutter has by default. We aim it at the `fonts/` shipped beside the binary (fontsDir "."),
  // falling back to the sibling texish checkout during development (fontsDir "../texish"). An explicit
  // `TEXISH_FONTS_DIR` in the environment wins — texish honours it directly, so we leave it be.
  private def ensureFonts(): Unit =
    if !fontsConfigured then
      if !sys.env.contains("TEXISH_FONTS_DIR") then
        Seq(".", "../texish").find(d => new File(d, "fonts").isDirectory).foreach(d => CairoTypesetter.fontsDir = d)
      fontsConfigured = true

  // Build a typesetter, feed it the document, flush it — the same three steps the texish CLI runs. The
  // engine ships only primitives; a card template pulls in higher-level macros with `\use{document}`.
  private def typeset(t: Typesetter, source: String): Unit =
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    proc.process(source)
    t.end()

  // A blank, fully transparent card — the fallback when a template ships no page — so `renderCard`'s
  // promise of a real file at the returned path always holds.
  private def writeTransparent(width: Int, height: Int, path: String): Unit =
    val surface = imageSurfaceCreate(Format.ARGB32, width, height)
    surface.flush()
    surface.writeToPNG(path)
    surface.destroy()
