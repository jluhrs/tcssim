import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Settings {

  object LibraryVersion {
    // Scala libraries
    lazy val catsEffect = "3.5.2"
    lazy val cats       = "2.10.0"
    lazy val mouse      = "1.2.2"
    lazy val fs2        = "3.9.2"
    lazy val kittens    = "3.1.0"

    lazy val munit           = "0.7.29"
    lazy val munitCatsEffect = "1.0.7"
    lazy val munitDiscipline = "1.0.9"

    // EPICS Libraries
    lazy val jca = "2.4.8"

    // Lucuma
    lazy val lucumaCore = "0.89.1"

    val slf4j    = "2.0.9"
    val log4s    = "1.10.0"
    val log4cats = "2.6.0"
    val logback  = "1.4.12"
    val janino   = "3.1.11"
  }

  val MUnit = Def.setting(
    Seq(
      "org.scalameta" %%% "munit"               % LibraryVersion.munit           % Test,
      "org.typelevel" %%% "munit-cats-effect-3" % LibraryVersion.munitCatsEffect % Test,
      "org.typelevel" %%% "discipline-munit"    % LibraryVersion.munitDiscipline % Test
    )
  )

  val EpicsJCA = Def.setting("org.epics" % "jca" % LibraryVersion.jca)

  val Cats         = Def.setting("org.typelevel" %%% "cats-core" % LibraryVersion.cats)
  val Kittens      = Def.setting("org.typelevel" %%% "kittens" % LibraryVersion.kittens)
  val CatsEffect   = Def.setting("org.typelevel" %%% "cats-effect" % LibraryVersion.catsEffect)
  val Fs2          = Def.setting("co.fs2" %% "fs2-core" % LibraryVersion.fs2)
  val Mouse        = Def.setting("org.typelevel" %%% "mouse" % LibraryVersion.mouse)
  val Log4Cats     = Def.setting("org.typelevel" %%% "log4cats-slf4j" % LibraryVersion.log4cats)
  val Logback      = Def.setting(
    Seq(
      "ch.qos.logback"      % "logback-core"    % LibraryVersion.logback,
      "ch.qos.logback"      % "logback-classic" % LibraryVersion.logback,
      "org.codehaus.janino" % "janino"          % LibraryVersion.janino
    )
  )
  val Log4s        = Def.setting("org.log4s" %%% "log4s" % LibraryVersion.log4s)
  val JuliSlf4j    = Def.setting("org.slf4j" % "jul-to-slf4j" % LibraryVersion.slf4j)
  val Logging      = Def.setting(Seq(JuliSlf4j.value, Log4s.value) ++ Logback.value)
  val Log4CatsNoop =
    Def.setting("org.typelevel" %%% "log4cats-noop" % LibraryVersion.log4cats % "test")

  // Lucuma libraries
  val LucumaCore = Def.setting(
    Seq(
      "edu.gemini" %%% "lucuma-core"         % LibraryVersion.lucumaCore,
      "edu.gemini" %%% "lucuma-core-testkit" % LibraryVersion.lucumaCore
    )
  )

}
