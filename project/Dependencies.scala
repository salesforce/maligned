import sbt._

object Dependencies {
  val kindProjector = "org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full
  val betterMonadicFor = "com.olegpy" %% "better-monadic-for" % "0.3.1"

  object cats {
    val org = "org.typelevel"

    val core = org %% "cats-core" % "2.0.0"
    val testKit = org %% "cats-testkit-scalatest" % "1.0.0-RC1"
    val scalacheck = "io.chrisdavenport" %% "cats-scalacheck" % "0.2.0"
    val effect = org %% "cats-effect" % "2.0.0"
  }
}
