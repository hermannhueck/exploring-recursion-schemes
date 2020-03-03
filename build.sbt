import Dependencies._
import ScalacOptions._

val projectName        = "Exploring Recursion Schemes"
val projectDescription = "Exploring Recursion Schemes"
val projectVersion     = "0.1.0"

val scala212               = "2.12.10"
val scala213               = "2.13.1"
val supportedScalaVersions = List(scala212, scala213)

inThisBuild(
  Seq(
    version := projectVersion,
    scalaVersion := scala212,
    crossScalaVersions := supportedScalaVersions,
    publish / skip := true,
    libraryDependencies ++= Seq(
      collectionCompat,
      silencerLib,
      silencerPlugin,
      kindProjectorPlugin,
      betterMonadicForPlugin
    ) ++ Seq(
      scalaTest,
      scalaCheck,
      scalaTestPlusCheck,
      scalaCheckShapeless,
      munit
    ).map(_ % Test),
    Test / parallelExecution := false,
    // S = Small Stack Traces, D = print Duration
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oSD"),
    // run 100 tests for each property // -s = -minSuccessfulTests
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-s", "100"),
    testFrameworks += new TestFramework("munit.Framework"),
    initialCommands :=
      s"""|
          |import scala.util.chaining._
          |println
          |""".stripMargin // initialize REPL
  )
)

lazy val root = (project in file("."))
  .aggregate(
    core,
    `olikitty-fby-examples`,
    `recursion-scheme-implementations`,
    `exploring-matryoshka`,
    `exploring-droste`
  )
  .settings(
    name := projectName,
    description := projectDescription,
    crossScalaVersions := Seq.empty
  )

lazy val core = (project in file("core"))
  .dependsOn(compat213, util)
  .settings(
    name := "core",
    description := "My gorgeous core App",
    scalacOptions ++= scalacOptionsFor(scalaVersion.value),
    console / scalacOptions := removeScalacOptionXlintUnusedForConsoleFrom(scalacOptions.value),
    libraryDependencies ++= Seq(
      shapeless,
      fs2Core,
      fs2Io
    )
  )

lazy val `olikitty-fby-examples` = (project in file("olikitty-fby-examples"))
  .dependsOn(compat213, util)
  .settings(
    name := "olikitty-fby-examples",
    description := "Oli Kitty's f(by) examples",
    scalacOptions ++= scalacOptionsFor(scalaVersion.value),
    console / scalacOptions := removeScalacOptionXlintUnusedForConsoleFrom(scalacOptions.value),
    libraryDependencies ++= Seq(fs2Io)
  )

lazy val `recursion-scheme-implementations` = (project in file("recursion-scheme-implementations"))
  .dependsOn(compat213, util)
  .settings(
    name := "recursion-scheme-implementations",
    description := "Demo implementations of recursion schemes",
    scalacOptions ++= scalacOptionsFor(scalaVersion.value),
    console / scalacOptions := removeScalacOptionXlintUnusedForConsoleFrom(scalacOptions.value)
  )

lazy val `exploring-matryoshka` = (project in file("exploring-matryoshka"))
  .dependsOn(compat213, util)
  .settings(
    name := "exploring-matryoshka",
    description := "Exploring Matryoshka recursion scheme library",
    scalaVersion := scala212, // matryoshka is not yet released for 2.13
    crossScalaVersions := Seq(scala212),
    scalacOptions ++= scalacOptionsFor(scalaVersion.value),
    console / scalacOptions := removeScalacOptionXlintUnusedForConsoleFrom(scalacOptions.value),
    libraryDependencies ++= Seq(matryoshkaCore)
  )

lazy val `exploring-droste` = (project in file("exploring-droste"))
  .dependsOn(compat213, util)
  .settings(
    name := "exploring-droste",
    description := "Exploring Droste recursion scheme library",
    scalacOptions ++= scalacOptionsFor(scalaVersion.value),
    console / scalacOptions := removeScalacOptionXlintUnusedForConsoleFrom(scalacOptions.value),
    libraryDependencies ++= Seq(drosteCore)
  )

lazy val compat213 = (project in file("compat213"))
  .settings(
    name := "compat213",
    description := "compat library providing features of Scala 2.13 backported to 2.12",
    scalacOptions ++= scalacOptionsFor(scalaVersion.value)
  )

lazy val util = (project in file("util"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "util",
    description := "Utilities",
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "build",
    scalacOptions ++= scalacOptionsFor(scalaVersion.value)
  )
