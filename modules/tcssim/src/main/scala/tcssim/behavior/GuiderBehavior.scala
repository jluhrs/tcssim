// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package tcssim.behavior

import cats.{Applicative, Monad, Parallel}
import cats.syntax.all.*
import monocle.Getter
import tcssim.{BinaryOnOff, GuideCmds, GuideStat, TcsEpicsDB, WfsCommands}

case class GuiderBehavior[F[_]: Monad: Parallel](
  guideCmdGetter: Getter[TcsEpicsDB[F], GuideCmds[F]],
  wfsCmdGetter: Getter[TcsEpicsDB[F], WfsCommands[F]],
  statusGetter: Getter[TcsEpicsDB[F], GuideStat[F]]
) extends Behavior[F] {
  override def process(db: TcsEpicsDB[F]): F[Unit] = List(
    guideCmdGetter.get(db).m2Guide.inputA.getOption.flatMap(
      _.flatMap{
        case "On" => BinaryOnOff.On.some
        case "Off" => BinaryOnOff.Off.some
        case _ => none
      }.map(statusGetter.get(db).m2GuideState.put)
        .getOrElse(Applicative[F].unit)
    ),
    guideCmdGetter.get(db).m1Guide.inputA.getOption.flatMap(
      _.flatMap {
          case "On" => BinaryOnOff.On.some
          case "Off" => BinaryOnOff.Off.some
          case _ => none
        }.map(statusGetter.get(db).m1GuideState.put)
        .getOrElse(Applicative[F].unit)
    )
  ).parSequence.void
}
