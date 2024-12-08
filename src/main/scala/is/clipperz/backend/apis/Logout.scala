package is.clipperz.backend.apis

import is.clipperz.backend.Exceptions.*
import is.clipperz.backend.LogAspect
import is.clipperz.backend.services.SessionManager

import java.util

import zio.{ ZIO, Cause }
import zio.http.{ Method, Path, Response, Request, Routes, handler }
import is.clipperz.backend.otel.TracingAspect

val logoutApi = Routes(
    Method.POST / "api" / "logout" -> handler: (request: Request) =>
        TracingAspect.endpointTracing:
            for {
                sessionManager <- ZIO.service[SessionManager]
                _              <- sessionManager.deleteSession(request)
            } yield Response.ok
)