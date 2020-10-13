import Dependencies._

val polish =
  taskKey[Unit]("Run automated source code changes such as Scalafix and Scalafmt rules.")

val lint = taskKey[Unit]("Run static linters on code")
val validate = taskKey[Unit]("Perform validation (compiles, tests pass, linter is happy, etc).")

// Projects
lazy val maligned = project
  .in(file("."))
  .aggregate(core)
  .settings(commonSettings)
  .settings(
    skip in publish := true
  )

lazy val core = project
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "maligned",
    libraryDependencies ++= Seq(
      cats.core,
      cats.effect % Test,
      cats.scalacheck % Test,
      cats.testKit % Test
    )
  )

// General Settings

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.4.2"

lazy val commonSettings = Seq(
  scalaVersion := "2.11.12",
  crossScalaVersions := Seq(scalaVersion.value, "2.12.12"),
  wartremoverErrors := Warts.allBut(
    // Maligned's heavy use of existential types seems to trigger a lot of claims about Any and
    // Nothing being inferred in places that are doing sound and reasonable things.
    Wart.Any,
    Wart.Nothing
  ),
  lint := {
    scalafmtCheckAll.value
    (Compile / scalafmtSbtCheck).value
    (Compile / scalafix).toTask(" --test").value
    (Test / scalafix).toTask(" --test").value
  },
  validate := {
    lint.value
    mimaReportBinaryIssues.value
    (test in Test).value
  },
  polish := {
    (Compile / scalafix).toTask("").value
    (Test / scalafix).toTask("").value
    (Compile / scalafmtSbt).value
    scalafmtAll.value
  },
  // required for scalafix
  semanticdbEnabled := true,
  semanticdbVersion := scalafixSemanticdb.revision,
  addCompilerPlugin(kindProjector),
  addCompilerPlugin(betterMonadicFor)
)

// General Settings
inThisBuild(
  List(
    organization := "com.salesforce",
    developers := List(
      Developer(
        "ceedubs",
        "Cody Allen",
        "cody.allen@salesforce.com",
        url("https://github.com/ceedubs"))
    ),
    homepage := Some(url("https://github.com/salesforce/maligned")),
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    pomIncludeRepository := { _ => false },
    scalacOptions in (Compile, doc) ++= Seq(
      "-groups",
      "-sourcepath",
      (baseDirectory in LocalRootProject).value.getAbsolutePath,
      "-doc-source-url",
      "https://github.com/salesforce/maligned/blob/v" + version.value + "€{FILE_PATH}.scala"
    )
  ))
