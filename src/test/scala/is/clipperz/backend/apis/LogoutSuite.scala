package is.clipperz.backend.apis

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths, FileSystems }
import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO, Task }
import zio.stream.{ ZStream, ZSink }
import zio.test.Assertion.{ nothing, isTrue }
import zio.test.{ ZIOSpecDefault, assertZIO, assertNever, assertTrue, assert, TestAspect }
import zio.json.EncoderOps
import zio.http.{ Version, Headers, Method, URL, Request, Body }
import zio.http.*
import is.clipperz.backend.Main
import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.bytesToHex
import is.clipperz.backend.functions.crypto.HashFunction
import java.nio.file.Path
import is.clipperz.backend.functions.FileSystem
import is.clipperz.backend.services.SaveBlobData
import is.clipperz.backend.services.PRNG
import is.clipperz.backend.services.SessionManager
import is.clipperz.backend.services.UserArchive
import is.clipperz.backend.services.BlobArchive
import is.clipperz.backend.services.TollManager
import is.clipperz.backend.services.SrpManager
import is.clipperz.backend.functions.Conversions.{ bytesToBigInt, bigIntToBytes }
import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.services.UserCard
import java.sql.Blob
import zio.test.ZIOSpec
import zio.Scope
import zio.ZLayer
import is.clipperz.backend.services.SRPStep1Data
import is.clipperz.backend.data.srp.RFCTestVector
import is.clipperz.backend.services.SRPStep1Response
import is.clipperz.backend.services.SRPStep2Data
import is.clipperz.backend.functions.SrpFunctions.SrpFunctionsV6a
import is.clipperz.backend.functions.SrpFunctions
import is.clipperz.backend.services.SRPStep2Response
import is.clipperz.backend.services.OneTimeShareArchive

object LogoutSpec extends ZIOSpecDefault:
  val app = Main.clipperzBackend
  val blobBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "blobs").nn
  val userBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "users").nn
  val oneTimeShareBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "one_time_share").nn

  val environment =
    PRNG.live ++
      SessionManager.live ++
      UserArchive.fs(userBasePath, 2, false) ++
      BlobArchive.fs(blobBasePath, 2, false) ++
      OneTimeShareArchive .fs(oneTimeShareBasePath, 2, false) ++
      ((UserArchive.fs(userBasePath, 2, false) ++ PRNG.live) >>> SrpManager.v6a()) ++
      (PRNG.live >>> TollManager.live)

  val sessionKey = "sessionKey"

  val logoutWithSession = Request(
    url = URL(!! / "logout"),
    method = Method.POST,
    headers = Headers((SessionManager.sessionKeyHeaderName, sessionKey)),
    body = Body.empty,
    version = Version.Http_1_1,
    remoteAddress = None
  )

  val logoutNoSession = Request(
    url = URL(!! / "logout"),
    method = Method.POST,
    headers = Headers.empty,
    body = Body.empty,
    version = Version.Http_1_1,
    remoteAddress = None
  )

  def spec = suite("LogoutApis")(
    test("logout - fail - no session key in header") {
      for {
        responseCode <- app.runZIO(logoutNoSession).map(response => response.status.code)
      } yield assertTrue(responseCode == 400)
    },
    test("logout - success") {
      for {
        sessionManager <- ZIO.service[SessionManager]
        _ <- sessionManager.getSession(sessionKey)
        responseCode <- app.runZIO(logoutWithSession).map(response => response.status.code)
      } yield assertTrue(responseCode == 200)
    },
    test("logout - success - no session in store") {
      for {
        responseCode <- app.runZIO(logoutWithSession).map(response => response.status.code)
      } yield assertTrue(responseCode == 200)
    },
  ).provideLayerShared(environment) @@
    TestAspect.sequential @@
    TestAspect.beforeAll(ZIO.succeed(FileSystem.deleteAllFiles(blobBasePath.toFile().nn))) @@
    TestAspect.afterAll(ZIO.succeed(FileSystem.deleteAllFiles(blobBasePath.toFile().nn)))
