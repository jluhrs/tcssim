// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package tcssim

import cats.effect.Resource
import tcssim.epics.EpicsServer

trait RotatorCmds[F[_]] {
  val park: CadRecord[F]
}

object RotatorCmds {

  private val rotatorParkSuffix: String = "rotPark"
  private val rotatorStopSuffix: String = "rotStop"
  private val rotatorMove: String       = "rotMove"

  case class RotatorCmdsImpl[F[_]] private (
    park: CadRecord[F],
    stop: CadRecord2[F],
    move: CadRecord1[F]
  ) extends RotatorCmds[F]

  def build[F[_]](server: EpicsServer[F], top: String): Resource[F, RotatorCmds[F]] = for {
    park <- CadRecord.build(server, top + rotatorParkSuffix)
    stop <- CadRecord2.build(server, top + rotatorStopSuffix)
    move <- CadRecord1.build(server, top + rotatorMove)
  } yield RotatorCmdsImpl(park, stop, move)

}
