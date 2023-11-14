// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package tcssim

import cats.Monad
import cats.effect.Resource
import tcssim.epics.EpicsServer

trait TcsCommands[F[_]] {
  val apply: ApplyRecord[F]
  val car: CarRecord[F]
  val wfsCmds: WfsCommands[F]
  val guiderTrackCommands: GuiderTrackCommands[F]
  val offsetCmds: OffsetCmds[F]
  val guideCmds: GuideCmds[F]
  val agCmds: AGCmds[F]
  val gemsCmd: GemsCmds[F]
  val altairCmds: AltairCmds[F]
  val sequenceCmds: SequenceCmds[F]
  val targetCmds: TargetCmds[F]
  val wavelenghtCmds: WavelengthCmds[F]
  val followCmds: FollowCmds[F]
  val configCmds: ConfigCmds[F]
  val nodchopCmds: NodChopCmds[F]
  val carouselModeCmd: CadRecord1[F]
  val mountCmds: MountCmds[F]
  val rotatorCmds: RotatorCmds[F]
  def cads: List[CadRecord[F]]
}

object TcsCommands {
  val ApplySuffix: String      = "apply"
  val CarSuffix: String        = "applyC"
  val CarouselModeName: String = "carouselMode"

  private case class TcsCommandsImpl[F[_]: Monad](
    apply:               ApplyRecord[F],
    car:                 CarRecord[F],
    wfsCmds:             WfsCommands[F],
    guiderTrackCommands: GuiderTrackCommands[F],
    offsetCmds:          OffsetCmds[F],
    guideCmds:           GuideCmds[F],
    agCmds:              AGCmds[F],
    gemsCmd:             GemsCmds[F],
    altairCmds:          AltairCmds[F],
    sequenceCmds:        SequenceCmds[F],
    targetCmds:          TargetCmds[F],
    wavelenghtCmds:      WavelengthCmds[F],
    followCmds:          FollowCmds[F],
    configCmds:          ConfigCmds[F],
    nodchopCmds:         NodChopCmds[F],
    carouselModeCmd:     CadRecord1[F],
    mountCmds:           MountCmds[F],
    rotatorCmds:         RotatorCmds[F],
    defocusCmds:         DeFocusCmds[F],
    pointingOrigin:      PointingOriginCmds[F]
  ) extends TcsCommands[F]:
    override def cads: List[CadRecord[F]] =
      List(
        wfsCmds.cads,
        guiderTrackCommands.cads,
        offsetCmds.cads,
        guideCmds.cads,
        agCmds.cads,
        gemsCmd.cads,
        altairCmds.cads,
        sequenceCmds.cads,
        targetCmds.cads,
        wavelenghtCmds.cads,
        followCmds.cads,
        configCmds.cads,
        nodchopCmds.cads,
        mountCmds.cads,
        rotatorCmds.cads,
        defocusCmds.cads,
        pointingOrigin.cads
      ).flatten :+ carouselModeCmd

  def build[F[_]: Monad](server: EpicsServer[F], top: String): Resource[F, TcsCommands[F]] =
    for {
      apply <- ApplyRecord.build(server, top + ApplySuffix)
      car   <- CarRecord.build(server, top + CarSuffix)
      wfsc  <- WfsCommands.build(server, top)
      gtc   <- GuiderTrackCommands.build(server, top)
      ofc   <- OffsetCmds.build(server, top)
      gdc   <- GuideCmds.build(server, top)
      agc   <- AGCmds.build(server, top)
      gms   <- GemsCmds.build(server, top)
      aoc   <- AltairCmds.build(server, top)
      seqc  <- SequenceCmds.build(server, top)
      tgsc  <- TargetCmds.build(server, top)
      wvlc  <- WavelengthCmds.build(server, top)
      folc  <- FollowCmds.build(server, top)
      cfgc  <- ConfigCmds.build(server, top)
      ncc   <- NodChopCmds.build(server, top)
      cm    <- CadRecord1.build(server, top + CarouselModeName)
      mc    <- MountCmds.build(server, top)
      rc    <- RotatorCmds.build(server, top)
      df    <- DeFocusCmds.build(server, top)
      po    <- PointingOriginCmds.build(server, top)
    } yield TcsCommandsImpl(apply,
                            car,
                            wfsc,
                            gtc,
                            ofc,
                            gdc,
                            agc,
                            gms,
                            aoc,
                            seqc,
                            tgsc,
                            wvlc,
                            folc,
                            cfgc,
                            ncc,
                            cm,
                            mc,
                            rc,
                            df,
                            po
    )
}
