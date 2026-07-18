import scala.scalanative.build.*

// kutter — a video editor built on the suit toolkit, previewing and playing video decoded through
// the MLT multimedia framework. A single Scala Native binary: it draws its UI through suit (which
// owns the SDL3 window, Cairo drawing, and input loop), decodes video and audio through the MLT
// binding (which links the Homebrew libmlt-7), and plays the audio through SDL3's audio stream.
//
// suit, sdl3, mlt, hocon and texish are consumed BY SOURCE from their sibling checkouts (a ProjectRef
// each), so changes to any of them — all under co-development alongside this app — are picked up on the
// next build with no publish step. hocon parses the batch lower-thirds import list; texish typesets a
// lower third's card, so a card can be expressed as a texish document instead of drawn by hand. logger
// is a stable published artifact taken from Maven Central.

ThisBuild / scalaVersion := "3.8.4"
ThisBuild / organization := "io.github.edadma"
ThisBuild / version      := "0.0.1"
// suit depends on a published sdl3 transitively while kutter source-depends on the sibling sdl3
// checkout for its audio API; the source project shadows the published one. Warn rather than error.
ThisBuild / evictionErrorLevel := Level.Warn

lazy val app = project
  .in(file("."))
  .enablePlugins(ScalaNativePlugin)
  .dependsOn(
    ProjectRef(file("../suit"), "suitNative"),
    ProjectRef(file("../sdl3"), "core"),
    ProjectRef(file("../mlt"), "mlt"),
    ProjectRef(file("../hocon"), "hoconNative"),
    ProjectRef(file("../texish"), "texishNative"),
  )
  .settings(
    name := "kutter",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:implicitConversions",
    ),
    // logger is stable and published; the co-developed libraries come from the ProjectRefs above.
    libraryDependencies += "io.github.edadma" %%% "logger" % "0.0.11",
    // The project file — media, lower thirds, styles — is serialized as JSON. zio-json derives the
    // codecs and is verified to link and round-trip on Scala Native.
    libraryDependencies += "dev.zio" %%% "zio-json" % "0.7.3",
    // The decoder runs on a thread of its own and hands frames to the UI thread, so the runtime
    // must keep multithreading enabled. Fast developer link: no LTO, debug mode.
    nativeConfig ~= { c =>
      c.withMultithreading(true).withLTO(LTO.none).withMode(Mode.debug).withGC(GC.immix)
    },
    run / fork := true,
  )
