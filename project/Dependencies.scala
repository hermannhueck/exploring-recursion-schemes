import sbt._

object Dependencies {

  lazy val collectionCompatVersion    = "2.1.4"
  lazy val shapelessVersion           = "2.3.3"
  lazy val catsVersion                = "2.1.1"
  lazy val catsEffectVersion          = "2.1.1"
  lazy val fs2Version                 = "2.2.2"
  lazy val monixVersion               = "3.1.0"
  lazy val zioVersion                 = "1.0.0-RC17"
  lazy val scalaTestVersion           = "3.1.1"
  lazy val scalaMockVersion           = "4.4.0"
  lazy val scalaCheckVersion          = "1.14.3"
  lazy val scalaCheckShapelessVersion = "1.2.5"
  lazy val scalaCheckDatetimeVersion  = "0.3.2"
  lazy val seleniumVersion            = "3.141.59"
  lazy val munitVersion               = "0.5.2"
  lazy val minitestVersion            = "2.7.0"
  lazy val utestVersion               = "0.7.4"
  lazy val fansiVersion               = "0.2.9"
  lazy val pprintVersion              = "0.5.9"
  lazy val drosteVersion              = "0.8.0"
  lazy val matryoshkaVersion          = "0.21.3"

  lazy val collectionCompat = "org.scala-lang.modules" %% "scala-collection-compat" % collectionCompatVersion
  lazy val shapeless        = "com.chuusai"            %% "shapeless"               % shapelessVersion
  lazy val catsEffect       = "org.typelevel"          %% "cats-effect"             % catsEffectVersion
  lazy val fs2Core          = "co.fs2"                 %% "fs2-core"                % fs2Version
  lazy val fs2Io            = "co.fs2"                 %% "fs2-io"                  % fs2Version
  lazy val monixExecution   = "io.monix"               %% "monix-execution"         % monixVersion
  lazy val zio              = "dev.zio"                %% "zio"                     % zioVersion
  lazy val zioStreams       = "dev.zio"                %% "zio-streams"             % zioVersion
  lazy val zioTest          = "dev.zio"                %% "zio-test"                % zioVersion
  lazy val zioTestSbt       = "dev.zio"                %% "zio-test-sbt"            % zioVersion
  lazy val scalactic        = "org.scalactic"          %% "scalactic"               % scalaTestVersion
  lazy val scalaTest        = "org.scalatest"          %% "scalatest"               % scalaTestVersion
  lazy val scalaTestApp     = "org.scalatest"          %% "scalatest-app"           % scalaTestVersion
  // lazy val scalaTestPlusCheck  = "org.scalatestplus"          %% "scalatestplus-scalacheck"    % "3.1.0.0-RC2"
  lazy val scalaTestPlusCheck  = "org.scalatestplus"          %% "scalacheck-1-14"             % "3.1.1.1"
  lazy val scalaMock           = "org.scalamock"              %% "scalamock"                   % scalaMockVersion
  lazy val scalaCheck          = "org.scalacheck"             %% "scalacheck"                  % scalaCheckVersion
  lazy val scalaCheckShapeless = "com.github.alexarchambault" %% "scalacheck-shapeless_1.14"   % scalaCheckShapelessVersion
  lazy val scalaCheckDatetime  = "com.47deg"                  %% "scalacheck-toolbox-datetime" % scalaCheckDatetimeVersion
  lazy val seleniumJava        = "org.seleniumhq.selenium"    % "selenium-java"                % seleniumVersion
  lazy val seleniumHtmlunit    = "org.seleniumhq.selenium"    % "selenium-htmlunit-driver"     % seleniumVersion
  lazy val seleniumFirefox     = "org.seleniumhq.selenium"    % "selenium-firefox-driver"      % seleniumVersion
  lazy val seleniumChrome      = "org.seleniumhq.selenium"    % "selenium-chrome-driver"       % seleniumVersion
  lazy val munit               = "org.scalameta"              %% "munit"                       % munitVersion
  lazy val minitest            = "io.monix"                   %% "minitest"                    % minitestVersion
  lazy val minitestLaws        = "io.monix"                   %% "minitest-laws"               % minitestVersion
  lazy val utest               = "com.lihaoyi"                %% "utest"                       % utestVersion
  lazy val fansi               = "com.lihaoyi"                %% "fansi"                       % fansiVersion
  lazy val pprint              = "com.lihaoyi"                %% "pprint"                      % pprintVersion
  lazy val matryoshkaCore      = "com.slamdata"               %% "matryoshka-core"             % matryoshkaVersion
  lazy val drosteCore          = "io.higherkindness"          %% "droste-core"                 % drosteVersion
  lazy val drosteMacros        = "io.higherkindness"          %% "droste-macros"               % drosteVersion
  lazy val drosteMeta          = "io.higherkindness"          %% "droste-meta"                 % drosteVersion
  lazy val drosteLaws          = "io.higherkindness"          %% "droste-laws"                 % drosteVersion

  // compilerPlugins
  lazy val silencerVersion         = "1.6.0"
  lazy val kindProjectorVersion    = "0.11.0"
  lazy val betterMonadicForVersion = "0.3.1"
  lazy val macroParadiseVersion    = "2.1.1"

  // FORMAT: OFF

  // https://github.com/ghik/silencer
  lazy val silencerLib = "com.github.ghik" % "silencer-lib" % silencerVersion % Provided cross CrossVersion.full
  lazy val silencerPlugin = compilerPlugin(
    "com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full
  )
  // https://github.com/typelevel/kind-projector
  lazy val kindProjectorPlugin = compilerPlugin(
    compilerPlugin("org.typelevel" % "kind-projector" % kindProjectorVersion cross CrossVersion.full)
  )
  // https://github.com/oleg-py/better-monadic-for
  lazy val betterMonadicForPlugin = compilerPlugin(
    compilerPlugin("com.olegpy" %% "better-monadic-for" % betterMonadicForVersion)
  )
  // https://github.com/scalamacros/paradise
  // https://docs.scala-lang.org/overviews/macros/paradise.html
  lazy val macroParadise = compilerPlugin(
    compilerPlugin("org.scalamacros" %% "paradise" % macroParadiseVersion cross CrossVersion.full)
    // use in 2.11 or 2.12
    // included in the compiler since 2.13
    // enabled by -Ymacro-annotations, see: ScalacOptions.scala
  )

  // FORMAT: ON
}
