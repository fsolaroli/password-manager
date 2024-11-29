package is.clipperz.backend.apis

import is.clipperz.backend.Exceptions.*
import is.clipperz.backend.LogAspect
import is.clipperz.backend.services.SessionManager

import java.util

import zio.{ ZIO, Cause }
import zio.http.{ Method, Path, Response, Request, Routes, handler }
import zio.telemetry.opentelemetry.tracing.Tracing

val logoutApi = Routes(
    Method.POST / "api" / "logout" -> handler: (request: Request) =>
        ZIO.serviceWithZIO[Tracing](tracing => tracing.span(s"${request.method} ${request.url.path}") {
            for {
                sessionManager <- ZIO.service[SessionManager]
                _              <- sessionManager.deleteSession(request)
            } yield Response.ok
        })
)