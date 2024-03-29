/*
 * Copyright (c) 2020, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root or https://opensource.org/licenses/MIT
 */
import Dependencies._

val polish =
  taskKey[Unit]("Run automated source code changes such as Scalafix and Scalafmt rules.")

val lint = taskKey[Unit]("Run static linters on code")
val validate = taskKey[Unit]("Perform validation (compiles, tests pass, linter is happy, etc).")

val githubRepoUrl = "https://github.com/salesforce/maligned"

// Projects
lazy val maligned = project
  .in(file("."))
  .aggregate(core, coreTests, docs, malignedScalacheck)
  .settings(commonSettings)
  .settings(
    skip in publish := true
  )

lazy val core = project
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "maligned-core",
    libraryDependencies += cats.core
  )

lazy val coreTests = project
  .in(file("core-tests"))
  .dependsOn(core, malignedScalacheck)
  .settings(commonSettings)
  .settings(
    name := "maligned-core-tests",
    skip in publish := true,
    libraryDependencies ++= Seq(
      cats.core % Test,
      cats.effect % Test,
      cats.scalacheck % Test,
      cats.testKit % Test
    )
  )

lazy val malignedScalacheck = project
  .in(file("scalacheck"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "maligned-scalacheck",
    libraryDependencies ++= Seq(
      cats.core,
      cats.scalacheck
    )
  )

lazy val docs = project
  .in(file("docs"))
  .settings(commonSettings)
  .enablePlugins(ParadoxPlugin, ScalaUnidocPlugin)
  .settings(
    name := "docs",
    skip in publish := true,
    paradoxTheme := Some(builtinParadoxTheme("generic")),
    (Compile / paradox / target) := crossTarget.value / "paradox" / "site",
    (ScalaUnidoc / unidoc / target) := (Compile / paradox / target).value / "api",
    (Compile / paradox) := (Compile / paradox).dependsOn(Compile / unidoc).value,
    validate := validate.dependsOn(Compile / paradox).value,
    (ScalaUnidoc / unidoc / scalacOptions) ++= Seq(
      "-Xfatal-warnings",
      "-groups",
      "-sourcepath",
      (baseDirectory in LocalRootProject).value.getAbsolutePath,
      "-doc-source-url",
      s"$githubRepoUrl/blob/v${version.value}€{FILE_PATH}.scala",
      "-diagrams"
    ),
    (Compile / paradoxProperties) ++= Map(
      "scaladoc.maligned.base_url" -> ".../api/com/salesforce",
      "organization" -> organization.value,
      "version" -> version.value,
      "sourceUrl" -> githubRepoUrl,
      // Paradox defaults to the 'master' branch but we use 'main'
      "github.base_url" -> s"$githubRepoUrl/tree/main"
    )
  )
  .dependsOn(core, malignedScalacheck)

// General Settings

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.4.2"

lazy val commonSettings = Seq(
  scalaVersion := "2.12.12",
  crossScalaVersions := Seq(scalaVersion.value, "2.11.12"),
  wartremoverErrors := Warts.allBut(
    // Maligned's heavy use of existential types seems to trigger a lot of claims about Any and
    // Nothing being inferred in places that are doing sound and reasonable things.
    Wart.Any,
    Wart.Nothing
  ),
  lint := {
    scalafmtCheckAll.value
    (Compile / headerCheck).value
    (Test / headerCheck).value
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
    (Compile / headerCreate).value
    (Test / headerCreate).value
    (Compile / scalafix).toTask("").value
    (Test / scalafix).toTask("").value
    (Compile / scalafmtSbt).value
    scalafmtAll.value
  },
  // required for scalafix
  semanticdbEnabled := true,
  semanticdbVersion := scalafixSemanticdb.revision,
  addCompilerPlugin(kindProjector),
  addCompilerPlugin(betterMonadicFor),
  headerLicense := Some(
    HeaderLicense.Custom(
      """|Copyright (c) 2020, salesforce.com, inc.
        |All rights reserved.
        |SPDX-License-Identifier: MIT
        |For full license text, see the LICENSE file in the repo root or https://opensource.org/licenses/MIT
        |""".stripMargin
    ))
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
    homepage := Some(url(githubRepoUrl)),
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    pomIncludeRepository := { _ => false }
  ))
