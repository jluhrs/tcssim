// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package tcssim.epics

import cats.effect.{ Async, Resource }
import cats.syntax.all._
import com.cosylab.epics.caj.cas.util.{ DefaultServerImpl, MemoryProcessVariable }
import fs2.Stream
import cats.effect.std.{ Dispatcher, Queue }
import gov.aps.jca.Monitor
import gov.aps.jca.cas.ProcessVariableEventCallback
import gov.aps.jca.dbr.{ DBR, DBRType, TIME }

trait MemoryPV[F[_], T] {
  val get: F[Array[T]]
  def put(v: Array[T]): F[Unit]
  def valueStream: F[Stream[F, Array[T]]]
}

object MemoryPV {

  trait ToDBRType[T] {
    type Dbr <: DBR
    val dbrType: DBRType
    def buildDBR(): Dbr with TIME
    def toEpicsVal(v:      Array[T]): Dbr
    def fromEpicsValue(v:  Dbr): Array[T]
    def extractOption(dbr: DBR): Option[Array[T]]
    def initValue(v: Array[T]): Object = v
    val enumLabels: Array[String]      = Array.empty
  }

  def build[F[_]: Async, T](server: DefaultServerImpl, name: String, init: Array[T])(implicit
    toDBRType:                      ToDBRType[T],
    dispatcher:                     Dispatcher[F]
  ): Resource[F, MemoryPV[F, T]] = for {
    q   <- Resource.eval(Queue.unbounded[F, Array[T]])
    mpv <- Resource.make {
             Async[F].delay {
               val m = new MemoryProcessVariable(
                 name,
                 new ProcessVariableEventCallback {
                   override def postEvent(select: Int, event: DBR): Unit =
                     if ((select & Monitor.VALUE) =!= 0)
                       toDBRType
                         .extractOption(event)
                         .map(x => dispatcher.unsafeRunAndForget(q.offer(x)))
                         .getOrElse(())
                     else ()

                   override def canceled(): Unit = ()
                 },
                 toDBRType.dbrType,
                 toDBRType.initValue(init)
               )
               m.setEnumLabels(toDBRType.enumLabels)
               server.registerProcessVaribale(m)
               m
             }
           } { x =>
             Async[F].delay {
               server.unregisterProcessVaribale(name)
               x.destroy()
             }
           }
  } yield new MemoryPVImpl[F, T](mpv, q)

  private class MemoryPVImpl[F[_]: Async: Dispatcher, T](
    mpv:       MemoryProcessVariable,
    q:         Queue[F, Array[T]]
  )(implicit
    toDBRType: ToDBRType[T]
  ) extends MemoryPV[F, T] {

    override val get: F[Array[T]] = Async[F].delay {
      val v: toDBRType.Dbr = toDBRType.buildDBR()
      mpv.read(v, null)
      toDBRType.fromEpicsValue(v)
    }

    override def put(v: Array[T]): F[Unit] = Async[F].delay {
      mpv.write(toDBRType.toEpicsVal(v), null)
    }

    override val valueStream: F[Stream[F, Array[T]]] =
      Async[F].delay(mpv.interestRegister()) *> Stream.fromQueueUnterminated(q).pure[F]
  }

}
