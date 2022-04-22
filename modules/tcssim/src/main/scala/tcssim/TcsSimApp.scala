// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package tcssim

import cats.effect.std.Dispatcher
import cats.effect.{ ExitCode, IO, IOApp }
import tcssim.epics.EpicsServer

object TcsSimApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val r = for {
      dsp <- Dispatcher[IO]
      srv <- EpicsServer.start(dsp)
      db  <- TcsEpicsDB.build(srv, "tc1:")
    } yield db

    r.use(_ => IO.never).as(ExitCode.Success)
  }
}
