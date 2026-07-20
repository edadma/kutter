package io.github.edadma.kutter

import zio.json.*

// Multicam: cutting one program picture between several synced sources — camera angles and full-frame
// title slides — while the sound of one chosen source plays through continuously underneath. It is an
// extension of the fixed-track model, not a parallel machine: a multicam program is an ordinary run of
// abutting clips on a video track (each clip one *cut*, stamped with the angle it shows), and the audio
// bed is an ordinary continuous clip on an audio track. `Player.buildGraph` compiles them like any other
// placements; the only thing it does specially is draw a title angle's card full-frame, because a title
// cut is treated as a video clip. What lives here is the *editing* logic the graph does not need: the
// group of synced angles, and the pure operation of switching the program angle at a frame.

/** What one multicam angle draws from. A `Clip` angle is a bin video source, synced by an `offset` — the
  * source frame that lines up with the group's first frame — so a cut can compute which slice of the
  * source plays at any program position. A `Title` angle is a full-frame card (the same name/title/style
  * a lower third carries, or a raw texish `body`); cutting to it shows that card as the whole picture. */
enum AngleSource:
  case Clip(clipId: String, offset: Int = 0)
  case Title(name: String, title: String, styleId: String = "broadcast-blue", body: Option[String] = None)

object AngleSource:
  given JsonCodec[AngleSource] = DeriveJsonCodec.gen

/** One angle in a multicam group: a labelled, synced source the program can cut to. */
final case class Angle(id: String, label: String, source: AngleSource)

object Angle:
  given JsonCodec[Angle] = DeriveJsonCodec.gen

/** A multicam group: a set of synced `angles` and which of them (`audioAngle`, a `Clip` index) supplies
  * the continuous audio bed. The angles share one timeline: a `Clip` angle's `offset` aligns its source
  * to the group's frame 0, so every angle names the same instant at the same program frame. Placing the
  * group lays a program of cuts on a video track and the audio bed on an audio track; switching an angle
  * re-cuts the program without touching the bed, which is why the sound never breaks. */
final case class Multicam(id: String, name: String, angles: List[Angle], audioAngle: Int = 0):
  /** The angle at `i`, if in range. */
  def angleAt(i: Int): Option[Angle] = angles.lift(i)

object Multicam:
  given JsonCodec[Multicam] = DeriveJsonCodec.gen

  /** A clip angle: a bin video source synced by `offset` (the source frame at the group's frame 0). */
  def clipAngle(label: String, clipId: String, offset: Int = 0): Angle =
    Angle(s"ang-${System.nanoTime()}", label, AngleSource.Clip(clipId, offset))

  /** A title angle: a full-frame card the program can cut to. */
  def titleAngle(label: String, name: String, title: String,
      styleId: String = "broadcast-blue", body: Option[String] = None): Angle =
    Angle(s"ang-${System.nanoTime()}", label, AngleSource.Title(name, title, styleId, body))

  /** A new group of `angles`, its audio bed the `audioAngle`-th angle. */
  def make(name: String, angles: List[Angle], audioAngle: Int = 0): Multicam =
    Multicam(s"mc-${System.nanoTime()}", name, angles, audioAngle)

  /** The source in-point a clip angle plays at program frame `cutStart`, given the program's start on the
    * timeline (`progStart`). It is the angle's sync `offset` shifted by how far into the program the cut
    * begins, so the same instant is read from every angle at the same program frame. A title angle has no
    * source, so its in-point is always 0 (a still card). Never negative. */
  def inPointFor(angle: Angle, progStart: Int, cutStart: Int): Int =
    angle.source match
      case AngleSource.Clip(_, offset) => math.max(0, offset + (cutStart - progStart))
      case _: AngleSource.Title        => 0

  /** Build one program cut of `angle` covering `[cutStart, cutStart + length)`. A clip angle's cut carries
    * the source id and the synced in-point; a title angle's cut carries no source (its card is resolved
    * from the group when the graph is built). Both are stamped with the group id and angle index so a
    * later switch can find and re-cut them. `angleIdx` must be in range; callers pass a validated index. */
  def makeCut(mc: Multicam, progStart: Int, cutStart: Int, length: Int, angleIdx: Int): PlacedClip =
    val a = mc.angles(angleIdx)
    a.source match
      case AngleSource.Clip(clipId, _) =>
        PlacedClip.make(clipId, cutStart, length, inPoint = inPointFor(a, progStart, cutStart))
          .copy(mc = Some(mc.id), angle = angleIdx)
      case _: AngleSource.Title =>
        PlacedClip.make("", cutStart, length, inPoint = 0).copy(mc = Some(mc.id), angle = angleIdx)

  /** The initial program for a freshly placed group: a single cut of `initialAngle` spanning the whole
    * placement. Switching angles later cuts this one segment up. */
  def initialProgram(mc: Multicam, progStart: Int, length: Int, initialAngle: Int = 0): List[PlacedClip] =
    List(makeCut(mc, progStart, progStart, length, initialAngle))

  /** The audio bed placement for a group: the audio angle's source, continuous across the whole program.
    * `None` when the group's audio angle is a title (a title carries no sound) — a caller then leaves the
    * program silent or picks another bed. */
  def audioBed(mc: Multicam, progStart: Int, length: Int): Option[PlacedClip] =
    mc.angleAt(mc.audioAngle).flatMap { a =>
      a.source match
        case AngleSource.Clip(clipId, offset) =>
          Some(PlacedClip.make(clipId, progStart, length, inPoint = math.max(0, offset))
            .copy(mc = Some(mc.id), angle = mc.audioAngle))
        case _: AngleSource.Title => None
    }

  /** Coalesce a program's cuts: any two abutting cuts of the same group showing the same angle become one.
    * The left cut absorbs the right (keeping its id, source, and in-point, extending its length), which is
    * exactly the continuous window the two covered — a clip angle's in-point advances in lock-step with the
    * program, so the joined cut plays the same frames the two did. Keeps the cuts in timeline order. */
  def mergeCuts(cuts: List[PlacedClip]): List[PlacedClip] =
    cuts.sortBy(_.timelineStart).foldLeft(List.empty[PlacedClip]) { (acc, c) =>
      acc match
        case prev :: rest if prev.mc == c.mc && prev.angle == c.angle && prev.timelineEnd == c.timelineStart =>
          prev.copy(length = prev.length + c.length) :: rest
        case _ => c :: acc
    }.reverse

  /** Build a group's angles from a set of bin sources, auto-synced by their audio. Each source is a
    * `(clipId, label, envelope)`; the `bedIdx`-th is the reference (offset 0, the audio bed), and every
    * other clip angle is offset by the lag at which its envelope best matches the reference's (see
    * [[AudioSync]]). A source with an empty envelope stays at offset 0 — nothing to correlate — so the
    * user can nudge it by hand. Title angles are added separately; this sizes only the camera angles. */
  def syncedAngles(sources: List[(String, String, Array[Float])], bedIdx: Int, maxLag: Int): List[Angle] =
    val bedEnv = sources.lift(bedIdx).map(_._3).getOrElse(Array.empty[Float])
    sources.zipWithIndex.map { case ((clipId, label, env), i) =>
      val offset = if i == bedIdx || env.isEmpty || bedEnv.isEmpty then 0 else AudioSync.syncOffset(bedEnv, env, maxLag)
      clipAngle(label, clipId, offset)
    }

  /** Place `mc`'s program on the timeline: the opening cut of `initialAngle` across `[atFrame, atFrame +
    * length)` on the video track `programTrackId`, and the continuous audio bed on `audioTrackId`. The
    * group is registered on the project if it is not already. The cuts and bed are appended to those
    * tracks, so a caller places into free space (a fresh program track is the clean home for a group). */
  def place(project: Project, mc: Multicam, atFrame: Int, length: Int,
      programTrackId: String, audioTrackId: String, initialAngle: Int = 0): Project =
    val cuts = initialProgram(mc, atFrame, length, initialAngle)
    val bed  = audioBed(mc, atFrame, length).toList
    val withGroup = if project.multicams.exists(_.id == mc.id) then project else project.copy(multicams = project.multicams :+ mc)
    withGroup
      .updateTrack(programTrackId)(t => t.copy(clips = t.clips ++ cuts))
      .updateTrack(audioTrackId)(t => t.copy(clips = t.clips ++ bed))

  /** Switch the program of group `mcId` to `angle` at timeline frame `atFrame`, across the project. The
    * group's cuts live on a video track; that track's cuts are re-cut through [[switchAt]] while its other
    * clips and — crucially — the audio bed on its audio track are left untouched, so the sound plays
    * through the picture cut. This is the one operation both a live switch (clicked during playback) and a
    * frame-precise switch (scrubbed to a frame, then clicked) drive. A missing group is a no-op. */
  def switchProgram(project: Project, mcId: String, atFrame: Int, angle: Int): Project =
    project.mcFor(mcId) match
      case None => project
      case Some(mc) =>
        project.copy(tracks = project.tracks.map { t =>
          if t.kind != MediaKind.Video then t
          else
            val groupCuts = t.clips.filter(_.mc.contains(mcId))
            if groupCuts.isEmpty then t
            else
              val progStart = groupCuts.map(_.timelineStart).min
              val others    = t.clips.filterNot(_.mc.contains(mcId))
              t.copy(clips = others ++ switchAt(groupCuts, mc, progStart, atFrame, angle))
        })

  /** Switch the program angle at frame `f`: from `f` up to the next existing cut boundary, show `angle`.
    * The cut containing `f` is split there — its left part keeps its old angle, its right part is re-cut to
    * the new angle (re-syncing a clip angle's in-point) — and same-angle neighbours are merged, so a switch
    * back to a running angle leaves no seam. A switch to the angle already showing at `f`, or a frame
    * outside the program, is a no-op. The audio bed is untouched, so the sound plays through the cut.
    *
    * `cuts` is one group's program (the cuts sharing `mc.id`); `progStart` is where that program begins on
    * the timeline. Pure — this is the core multicam edit, driven the same way by a live switch during
    * playback and a frame-precise switch while paused. */
  def switchAt(cuts: List[PlacedClip], mc: Multicam, progStart: Int, f: Int, angle: Int): List[PlacedClip] =
    val ordered = cuts.sortBy(_.timelineStart)
    ordered.find(c => f >= c.timelineStart && f < c.timelineEnd) match
      case None => ordered // the frame is off the program; nothing to cut
      case Some(cut) =>
        if cut.angle == angle then ordered // already showing this angle here
        else
          val before   = ordered.filter(_.timelineEnd <= cut.timelineStart)
          val after     = ordered.filter(_.timelineStart >= cut.timelineEnd)
          val leftPart  = if f > cut.timelineStart then List(cut.copy(length = f - cut.timelineStart)) else Nil
          val rightPart = List(makeCut(mc, progStart, f, cut.timelineEnd - f, angle))
          mergeCuts(before ++ leftPart ++ rightPart ++ after)
