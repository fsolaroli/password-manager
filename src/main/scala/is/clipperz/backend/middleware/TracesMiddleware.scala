package is.clipperz.backend.middleware

import io.opentelemetry.api.baggage.Baggage
import io.opentelemetry.api.baggage.propagation.W3CBaggagePropagator
import io.opentelemetry.context.propagation.TextMapGetter

import zio.Trace
import zio.ZIO
import zio.ZIOAspect
import zio.http.*
import zio.telemetry.opentelemetry.tracing.Tracing

import java.lang
import scala.jdk.CollectionConverters.IterableHasAsJava
import io.opentelemetry.api.common.Attributes
import io.opentelemetry.semconv.HttpAttributes
import io.opentelemetry.api.trace.TraceId
import io.opentelemetry.api.trace.Span
import io.opentelemetry.semconv.UrlAttributes
import io.opentelemetry.semconv.JvmAttributes
import io.opentelemetry.sdk.internal.AttributesMap
import io.opentelemetry.api.trace.Tracer
import io.opentelemetry.api.trace.TracerProvider
import io.opentelemetry.sdk.trace.SdkTracerProvider
import io.opentelemetry.api.OpenTelemetry
import io.opentelemetry.context.ContextStorage
import zio.ZLayer
import java.util.concurrent.TimeUnit
import zio.Clock
import io.opentelemetry.api.trace.StatusCode

def trace(instrumentationScopeName: String): Middleware[OpenTelemetry] =
	Middleware.interceptHandlerStateful(
		Handler.fromFunctionZIO[Request] { request =>
			ZIO.serviceWithZIO[OpenTelemetry](openTelemetry => 
				for {
					nanos	<-	Clock.currentTime(TimeUnit.NANOSECONDS)
					tracer	<- 	ZIO.succeed(openTelemetry.tracerBuilder(instrumentationScopeName).build.nn)
					span 	<- 	ZIO.succeed(
									tracer
										.spanBuilder(s"${request.method} ${request.url.path}")
										.setAllAttributes(
											Attributes
												.builder()
												.put(HttpAttributes.HTTP_REQUEST_METHOD, request.method.toString)
												.put(HttpAttributes.HTTP_ROUTE,		     request.url.toString)
												.build()
										)
										.setStartTimestamp(nanos, TimeUnit.NANOSECONDS)
										.startSpan()
								)
				} yield(span, (request, ()))
			)
		}
	)(
		Handler.fromFunctionZIO[(Span, Response)] { case (span, response) =>
			for {
				nanos <- Clock.currentTime(TimeUnit.NANOSECONDS)
				_     <- ZIO.succeed(
							span
								.setStatus(StatusCode.OK)
								.setAttribute(HttpAttributes.HTTP_RESPONSE_STATUS_CODE, response.status.code.toLong)
								.end(nanos, TimeUnit.NANOSECONDS))
			} yield (response)
		}
	)