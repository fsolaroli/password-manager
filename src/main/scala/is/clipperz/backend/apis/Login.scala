package is.clipperz.backend.apis

import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.LogAspect
import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.{ fromStream }
import is.clipperz.backend.services.{ SessionManager, SrpManager, SRPStep1Data, SRPStep2Data, SRPStep1Response }
import is.clipperz.backend.Exceptions.*

import zio.{ Cause, Chunk, Task, ZIO, durationInt }
import zio.metrics.Metric
import zio.http.{ Method, Path, Response, Request, Routes, Status, handler }
import zio.http.codec.PathCodec.string
import zio.http.Header.HeaderType
import zio.http.endpoint.Endpoint
import zio.http.endpoint.openapi.OpenAPIGen
import zio.json.{ EncoderOps, JsonEncoder }
import zio.json.ast.Json
import zio.http.RoutePattern
import is.clipperz.backend.services.SessionManager.sessionHeaderCodec
import zio.http.MediaType
import is.clipperz.backend.services.TollManager.tollReceiptHeader
import is.clipperz.backend.services.TollManager.tollReceiptHeaderCodec
import zio.http.codec.HttpCodecError
import zio.http.codec.HttpCodec
import is.clipperz.backend.services.TollManager.tollChallangeHeaderCodec
import is.clipperz.backend.services.TollManager.tollCostHeaderCodec
import zio.stream.ZStream
import zio.stream.ZSink
import zio.http.codec.HeaderCodec
import zio.http.Header
import zio.http.Header.ContentEncoding
import zio.http.Header.AcceptEncoding

val loginStep1 = Endpoint(RoutePattern.POST / "api" / "login" / "step1" / string("c"))
                .inStream[SRPStep1Data]
                .header(HeaderCodec.contentType.expect(Header.ContentType(MediaType.application.json)))
                .header(HeaderCodec.acceptEncoding.expect(Header.AcceptEncoding(AcceptEncoding.GZip(), AcceptEncoding.Deflate(), AcceptEncoding.Br(), AcceptEncoding.Unknown("zstd",None))))
                .header(sessionHeaderCodec)
                // .header(tollReceiptHeaderCodec)
                .header(HeaderCodec.accept.expect(Header.Accept(MediaType.application.json)))
                .out[SRPStep1Response]
                // .outHeader(sessionHeaderCodec)
                // .outHeader(tollChallangeHeaderCodec)
                // .outHeader(tollCostHeaderCodec)
                // .outHeader(tollReceiptHeaderCodec)
                .outErrors[Throwable](
                    HttpCodec.error[BadRequestException](Status.BadRequest),
                    HttpCodec.error[ResourceNotFoundException](Status.NotFound),
                )

val loginStep1Route = 
    loginStep1.implementHandler[SessionManager & SrpManager](
        handler: (c: String, loginStep1Stream: ZStream[Any, Nothing, SRPStep1Data], sessionKey: String) =>
            ZIO
            .service[SessionManager]
            .zip(ZIO.service[SrpManager])
            .zip(loginStep1Stream.runLast().flatMap(opt => ZIO.attempt(opt.get)))
            // .zip(fromStream[SRPStep1Data](loginStep1Stream))
            .flatMap((sessionManager, srpManager, loginStep1Data) =>
                if HexString(c) == loginStep1Data.c then
                    for {
                        session <- sessionManager.getSession(sessionKey) // create new session
                        (step1Response, session) <- srpManager.srpStep1(loginStep1Data, session)
                        _ <- sessionManager.saveSession(session)
                    } yield step1Response
                else ZIO.fail(new BadRequestException("c in request path differs from c in request body "))
        )
    )

val loginApi: Routes[SessionManager & SrpManager, Throwable] = Routes(
    Method.POST / "api" / "login" / "step1" / string("c") -> handler: (c: String, request: Request) =>
        ZIO
        .service[SessionManager]
        .zip(ZIO.service[SrpManager])
        .zip(ZIO.succeed(request.body.asStream))
        .flatMap((sessionManager, srpManager, content) =>
            fromStream[SRPStep1Data](content)
                .flatMap { loginStep1Data =>
                    if HexString(c) == loginStep1Data.c then
                        for {
                            session <- sessionManager.getSessionFromRequest(request) // create new session
                            (step1Response, session) <- srpManager.srpStep1(loginStep1Data, session)
                            _ <- sessionManager.saveSession(session)
                        } yield step1Response
                    else ZIO.fail(new BadRequestException("c in request path differs from c in request body "))
                }
        )
        .map(step1Response => Response.json(step1Response.toJson))
        @@ LogAspect.logAnnotateRequestData(request)
,
    // loginStep1Route,
    Method.POST / "api" / "login" / "step2" / string("c") -> handler: (c: String, request: Request) =>
        ZIO
        .service[SessionManager]
        .zip(ZIO.service[SrpManager])
        .zip(ZIO.succeed(request.body.asStream))
        .flatMap((sessionManager, srpManager, content) =>
            fromStream[SRPStep2Data](content)
                .flatMap { loginStep2Data =>
                for {
                    session <- sessionManager.getSessionFromRequest(request)
                    (step2Response, session) <- srpManager.srpStep2(loginStep2Data, session)
                    _ <- sessionManager.saveSession(session)
                } yield step2Response
                }
        )
        .map(step2Response => Response.json(step2Response.toJson))
        @@ LogAspect.logAnnotateRequestData(request)
)