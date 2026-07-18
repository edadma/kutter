// kutter is a Scala Native application: a suit GUI that previews video decoded through MLT. It
// links against the local suit and mlt checkouts by source (so toolkit and binding changes flow
// straight in without a publish step). The Scala Native plugin version matches suit's and mlt's so
// the NIR the linker stitches together is produced by one compiler-plugin version.
addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % "0.5.12")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
