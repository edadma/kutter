package io.github.edadma.kutter

import io.github.edadma.hocon.*

// The batch importer — the automation kutter exists for. A producer writes a plain list of
// name/title/in/out once, hands the file to kutter, and gets every faded lower third placed in one
// step, instead of building them one at a time in the inspector.
//
// The list is HOCON (the user's own cross-platform parser): human-friendly, commentable, and read
// straight through a `lowerThirds` array of objects. Times are the friendly part — a producer writes
// timecodes off a script ("0:03") or plain seconds, not frame numbers; the importer converts to
// frames at the project's frame rate. Everything here is pure (text in, `LowerThird`s or an error
// message out), so it is checked headlessly by the `KUTTER_PROBE_HIT` self-test.

/** Turns a HOCON batch list into `LowerThird`s ready to drop into a project.
  *
  * The document is a `lowerThirds` array of objects:
  * {{{
  * lowerThirds = [
  *   { name = "Jane Smith", title = "CEO, Acme", in = "0:03", out = "0:08" }
  *   { name = "Bob Lee",    title = "CTO, Acme", in = "1:12", out = "1:18", fade = 8, style = minimal }
  * ]
  * }}}
  * `name`, `in`, and `out` are required; `title` defaults to empty, `fade` to the model default, and
  * `style` to the style supplied by the caller (the project's first). A row may also carry a `body` — a
  * raw texish document (write it as a HOCON triple-quoted `"""…"""` string) that becomes this card's
  * whole look, overriding its style. Times are timecodes (`m:ss`, `m:ss.cc`, `h:mm:ss` — quote them,
  * since HOCON reads `:` as a separator) or plain seconds (`3`, `3.5`, `3.5s`). A row whose `out` is not
  * after its `in`, or whose time doesn't parse, fails the whole import with a message naming the row.
  */
object BatchImport:

  /** Parse a time written as a timecode (`h:mm:ss`, `m:ss`, `m:ss.cc`) or as seconds (`3`, `3.5`,
    * `3.5s`) into a number of seconds, or an error describing what didn't parse. */
  def parseTime(text: String): Either[String, Double] =
    val t = text.trim
    if t.isEmpty then Left("empty time")
    else if t.contains(':') then
      val fields = t.split(':').toList.map(_.trim)
      // Two fields are minutes:seconds, three are hours:minutes:seconds; the seconds field alone may
      // carry a fraction (m:ss.cc), the others are whole.
      val parsed = fields match
        case m :: s :: Nil      => (Some(0), m.toIntOption, s.toDoubleOption)
        case h :: m :: s :: Nil => (h.toIntOption, m.toIntOption, s.toDoubleOption)
        case _                  => (None, None, None)
      parsed match
        case (Some(h), Some(m), Some(s)) if h >= 0 && m >= 0 && s >= 0 => Right(h * 3600 + m * 60 + s)
        case _                                                         => Left(s"not a timecode: '$text'")
    else
      // Bare seconds, with an optional trailing `s` (but not `ms`, which is not a lower-third unit).
      val body = if t.endsWith("s") && !t.endsWith("ms") then t.dropRight(1).trim else t
      body.toDoubleOption match
        case Some(s) if s >= 0 => Right(s)
        case _                 => Left(s"not a time: '$text'")

  /** Parse `text` into lower thirds, converting each in/out to frames at `fps` and defaulting a row's
    * missing style to `defaultStyle`. Returns the built overlays, or the first error found. */
  def parse(text: String, fps: Double, defaultStyle: String): Either[String, List[LowerThird]] =
    try
      val cfg = Hocon.parse(text)
      if !cfg.hasPath("lowerThirds") then Left("no `lowerThirds` list in the file")
      else
        val rows = cfg.getList("lowerThirds").zipWithIndex.map {
          case (o: ConfigObject, i) => rowToLt(Config(o), i, fps, defaultStyle)
          case (_, i)               => Left(s"lowerThirds[$i] is not an object")
        }
        sequence(rows)
    catch case e: Exception => Left(if e.getMessage != null then e.getMessage else e.toString)

  // One array element → one `LowerThird`, or an error naming the row.
  private def rowToLt(c: Config, index: Int, fps: Double, defaultStyle: String): Either[String, LowerThird] =
    def required(name: String): Either[String, String] =
      c.getStringOpt(name).toRight(s"lowerThirds[$index]: missing `$name`")
    for
      name     <- required("name")
      inStr    <- required("in")
      outStr   <- required("out")
      inSec    <- parseTime(inStr).left.map(e => s"lowerThirds[$index].in: $e")
      outSec   <- parseTime(outStr).left.map(e => s"lowerThirds[$index].out: $e")
      inFrame   = math.round(inSec * fps).toInt
      outFrame  = math.round(outSec * fps).toInt
      _        <- Either.cond(outFrame > inFrame, (), s"lowerThirds[$index]: out ($outStr) must be after in ($inStr)")
    yield
      val base = LowerThird(
        id       = c.getStringOpt("id").getOrElse(s"batch-${System.nanoTime()}-$index"),
        name     = name,
        title    = c.getStringOpt("title").getOrElse(""),
        inFrame  = inFrame,
        outFrame = outFrame,
        styleId  = c.getStringOpt("style").getOrElse(defaultStyle),
        // A `body` is a raw texish document for this one card (a HOCON triple-quoted string), overriding
        // its style's look; omitted, the style draws the card.
        body     = c.getStringOpt("body"),
      )
      // Only override the model's fade default when the row states one.
      c.getIntOpt("fade").map(f => base.copy(fadeFrames = f)).getOrElse(base)

  // Collapse a list of per-row results into one: all the overlays, or the first error.
  private def sequence(rows: List[Either[String, LowerThird]]): Either[String, List[LowerThird]] =
    rows.foldRight[Either[String, List[LowerThird]]](Right(Nil)) { (row, acc) =>
      for lt <- row; rest <- acc yield lt :: rest
    }
