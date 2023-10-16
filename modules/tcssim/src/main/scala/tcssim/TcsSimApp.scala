// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package tcssim

import cats.syntax.all.*
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Resource
import cats.effect.std.Dispatcher
import cats.implicits.catsSyntaxEq
import fs2.Stream
import monocle.Getter
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import tcssim.behavior.{Behavior, TargetBehavior}
import tcssim.epics.EpicsServer

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

object TcsSimApp extends IOApp {

  private implicit def L: Logger[IO] = Slf4jLogger.getLoggerFromName[IO]("tcssim")

  override def run(args: List[String]): IO[ExitCode] = {
    val r = for {
      _   <- Resource.eval(printBanner)
      dsp <- Dispatcher.parallel[IO]
      srv <- EpicsServer.start[IO](dsp)
      db  <- TcsEpicsDB.build(srv, "tc1:")
    } yield db

    r.use(process).as(ExitCode.Success)
  }

  def process(db: TcsEpicsDB[IO]): IO[Unit] =
    (db.process, db.commands.apply.DIR.valueStream.map(_.evalMap(_.map(carActivity(db)).getOrElse(IO.unit))))
      .mapN{_ :+ _}
      .use { Stream.emits[IO, Stream[IO, Unit]](_).parJoinUnbounded.compile.drain }

  val BusyTime: FiniteDuration = 1.seconds

  def carActivity(db: TcsEpicsDB[IO])(dir: CadDirective): IO[Unit] =
    if (dir === CadDirective.START)
      for {
        clid <- db.commands.apply.CLID.getOption.map(_.getOrElse(0))
        _    <- db.commands.apply.CLID.put(clid + 1)
        _    <- db.commands.apply.VAL.put(clid + 1)
        _    <- db.commands.apply.MESS.put("")
        _    <- db.commands.car.CLID.put(clid + 1)
        _    <- db.commands.car.OMSS.put("")
        _    <- db.commands.car.VAL.put(CarState.BUSY)
        _    <- runBehaviors(db)
        _    <- db.clean
        _    <- IO.sleep(BusyTime)
        _    <- db.commands.car.VAL.put(CarState.IDLE)
      } yield ()
    else IO.unit

  def printBanner[F[_]: Logger]: F[Unit] = {
    val banner = """
  ______             _____ _
 /_  __/_________   / ___/(_)___ ___
  / / / ___/ ___/   \__ \/ / __ `__ \
 / / / /__(__  )   ___/ / / / / / / /
/_/  \___/____/   /____/_/_/ /_/ /_/

"""
    Logger[F].info(banner)
  }


  private val behaviors: List[Behavior[IO]] = List(
    TargetBehavior[IO](Getter(_.commands.targetCmds.sourceA), Getter(_.status.targets.sourceATarget)),
    TargetBehavior[IO](Getter(_.commands.targetCmds.sourceB), Getter(_.status.targets.sourceBTarget)),
    TargetBehavior[IO](Getter(_.commands.targetCmds.sourceC), Getter(_.status.targets.sourceCTarget)),
    TargetBehavior[IO](Getter(_.commands.targetCmds.pwfs1), Getter(_.status.targets.pwfs1Target)),
    TargetBehavior[IO](Getter(_.commands.targetCmds.pwfs2), Getter(_.status.targets.pwfs2Target)),
    TargetBehavior[IO](Getter(_.commands.targetCmds.oiwfs), Getter(_.status.targets.oiwfsTarget)),
    TargetBehavior[IO](Getter(_.commands.targetCmds.g1), Getter(_.status.targets.g1Target)),
    TargetBehavior[IO](Getter(_.commands.targetCmds.g2), Getter(_.status.targets.g2Target)),
    TargetBehavior[IO](Getter(_.commands.targetCmds.g3), Getter(_.status.targets.g3Target)),
    TargetBehavior[IO](Getter(_.commands.targetCmds.g4), Getter(_.status.targets.g4Target))
  )


  private def runBehaviors(db: TcsEpicsDB[IO]): IO[Unit] = behaviors.map(_.process(db)).parSequence.void

}
