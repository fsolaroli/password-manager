package is.clipperz.backend.services

import zio.{ ZIO, Task, Layer, ZLayer, Tag }
import zio.internal.stacktracer.Tracer
import zio.telemetry.opentelemetry.tracing.Tracing
import is.clipperz.backend.otel.TracingAspect

trait PRNG:
  def nextBytes(size: Int): Task[Array[Byte]]

object PRNG:
  case class BasicPRNG(tracing: Tracing) extends PRNG:
    override def nextBytes(size: Int): Task[Array[Byte]] =
      ZIO.attempt(Array.fill(size)((scala.util.Random.nextInt(256) - 128).toByte))
      @@ TracingAspect.methodTracing("nextBytes", tracing)

  val live: ZLayer[Tracing, Nothing, PRNG] =
    ZLayer(ZIO.serviceWith[Tracing](new BasicPRNG(_)))
