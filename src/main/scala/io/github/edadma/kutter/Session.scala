package io.github.edadma.kutter

import java.io.{File, PrintWriter}
import scala.io.Source
import zio.json.*

/** What the app opens with: the project and, if it came from a `.kutter` file, that file's path (so
  * Save writes back to it and the title bar names it). */
private[kutter] final case class Session(project: Project, path: Option[String])

private[kutter] object Session:
  given JsonCodec[Session] = DeriveJsonCodec.gen

  /** The session the app opens with, resolved from the command-line `args` (MLT must be initialised — a
    * media argument is probed for its length and format). A `.kutter` argument opens `loadedProject`
    * (already read by the caller) bound to its file. A bare media path resumes the remembered session
    * when it already holds that clip — so reopening reloads the work done last time — otherwise it starts
    * a fresh project that adopts the clip's own format. With no argument it resumes whatever session was
    * remembered, or opens blank. There is no hardcoded project; the cache is the app's memory. */
  def resolve(args: Seq[String], loadedProject: Project): Session =
    args.headOption match
      case Some(a) if a.endsWith(".kutter") => Session(loadedProject, Some(a))
      case Some(mediaArg) =>
        SessionStore.load()
          .filter(_.project.bin.exists(_.path == mediaArg))
          .getOrElse {
            val spec = Player.probeSpec(mediaArg)
            Session(Diagnostics.videoProject(mediaArg, Player.mediaLength(mediaArg, spec)).withSpec(spec), None)
          }
      case None =>
        SessionStore.load().getOrElse(Session(Project.blank, None))

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
