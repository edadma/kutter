package io.github.edadma.kutter

import java.io.{File, PrintWriter}
import scala.io.Source
import zio.json.*

/** What the app opens with: the project and, if it came from a `.kutter` file, that file's path (so
  * Save writes back to it and the title bar names it). */
private[kutter] final case class Session(project: Project, path: Option[String])

private[kutter] object Session:
  given JsonCodec[Session] = DeriveJsonCodec.gen

/** Remembers the working session between runs, in the cache area. The app has no hardcoded project: it
  * starts blank and writes the session here on every change, so reopening it — with the same clip, or
  * with no argument at all — reloads exactly what was on screen, its bound `.kutter` path included. */
private[kutter] object SessionStore:
  private val file = new File(new File(System.getProperty("user.home"), ".cache/kutter"), "session.json")

  /** The remembered session, or `None` if there is none yet or it cannot be read. */
  def load(): Option[Session] =
    if !file.exists() then None
    else
      val src = Source.fromFile(file)
      try src.mkString.fromJson[Session].toOption
      finally src.close()

  /** Persist `session` as the one to reopen next time. */
  def save(session: Session): Unit =
    file.getParentFile.mkdirs()
    val w = new PrintWriter(file)
    try w.write(session.toJsonPretty)
    finally w.close()
