package is.clipperz.backend.otel

import zio.ZIOAspect
import zio.{ URIO, ZIO }
import zio.telemetry.opentelemetry.tracing.Tracing
import zio.Trace
import zio.Supervisor
import zio.telemetry.opentelemetry.OpenTelemetry
import zio.telemetry.opentelemetry.common.Attributes
import zio.telemetry.opentelemetry.common.Attribute
import io.opentelemetry.api.common.AttributeKey

object TracingAspect:
    // def recursiveTracing[R, E, A](spanName: String): ZIOAspect[Nothing, R with Tracing, E, A] =
    //     new ZIOAspect[Nothing, R with Tracing, E, A] {
    //     override def apply[R1 <: R with Tracing, E1 >: E, A1 >: A](zio: ZIO[R1, E1, A1]): ZIO[R1, E1, A1] =
    //         Tracing.span(spanName)(zio.flatMap {
    //         case result: ZIO[_, _, _] => // Check if the result itself is an effect
    //             result @@ traceEffect(spanName) // Recursively trace it
    //         case other => ZIO.succeed(other) // If not an effect, simply return it
    //         })
    //     }

    // def apply(): ZIOAspect[Nothing, Tracing, Any, Any, Any, Any] = {
    //     new ZIOAspect[Nothing, Tracing, Any, Any, Any, Any] {
    //         override def apply[R, E, A](zio: ZIO[R, E, A])(implicit trace: Trace): ZIO[R & Tracing, E, A] = {
    //             val spanName = s"Effect ${trace}"
    //             ZIO.serviceWithZIO[Tracing](_.span(spanName)(zio))
    //         }
    //     }
    // }  

    def endpointTracing[R, E, A](zio: ZIO[R & Tracing, E, A]): ZIO[R & PropagatorProvider & Tracing, E, A] =
        ZIO
        .service[Tracing]
        .zip(ZIO.service[PropagatorProvider])
        .flatMap((tracing, propagatorProvider) => {
            tracing.extractSpan(
                propagatorProvider.getTracePropagator()
            ,   propagatorProvider.getIncomingCarrier()
            ,   s"handler"
            ) (zio)
        })

    def methodTracing(spanName: String, tracing: Tracing): ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] =
        new ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any]:
            override def apply[R, E, A](zio: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
                tracing.span(spanName)(zio)


    // def recursiveTracing(zio: ZIO[?, ?, ?]): ZIO[Tracing, ?, ?] =
    //     ZIO.descriptorWith { descriptor =>
    //         ZIO.serviceWithZIO[Tracing](_.span(descriptor.id.id.toString())(zio.flatMap { res =>
    //             if res.isInstanceOf[ZIO[?, ?, ?]] then
    //                 recursiveTracing(res)
    //             else
    //                 ZIO.succeed(res)
    //             //  _ match
    //             // case res: ZIO[Any, Any, Any] => recursiveTracing(res)
    //             // case other => ZIO.succeed(other)

    //         }))
    //     }
    
    // def recursiveTracing[R, E, A](zio: ZIO[R, E, A]): ZIO[Tracing & R, E, A] =
    //     ZIO.descriptorWith { descriptor =>
    //         ZIO.serviceWithZIO[Tracing] { tracing =>
    //         tracing.span(descriptor.id.id.toString()):
    //             zio.flatMap { res =>
    //                 if res.isInstanceOf[ZIO[?, ?, ?]] then
    //                     recursiveTracing(res.asInstanceOf[ZIO[R, E, A]])
    //                 else
    //                     ZIO.succeed(res)
    //             }.tap(res => ZIO.debug(s"RESULT => ${res}"))
    //     }
    // }

    // private def traceEffect: ZIOAspect[Tracing, Any, Any, Any, Any, Any] =
    //     new ZIOAspect[Tracing, Any, Any, Any, Any, Any] {
    //         override def apply[R, E, A](zio: ZIO[R & Tracing, E, A]): ZIO[R & Tracing, E, A] =
    //             ZIO.descriptorWith { descriptor =>
    //             ZIO.serviceWithZIO[Tracing](_.span(descriptor.id.id.toString())(zio))
    //             }
    //     }
    
    // private def traceEffect[R1]: ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] =
    //     new ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] {
    //         override def apply[R1, E, A](zio: ZIO[R1, E, A])(implicit trace: Trace): ZIO[R1, E, A] =
    //         ZIO.descriptorWith { descriptor =>
    //             ZIO.serviceWithZIO[Tracing](_.span(descriptor.id.id.toString())(zio))
    //         }
    //     }
