package is.clipperz.backend.apis

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths, FileSystems }
import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO }
import zio.stream.{ ZStream, ZSink }
import zio.test.Assertion.nothing
import zio.test.{ ZIOSpecDefault, assertTrue, assert, TestAspect }
import zio.json.EncoderOps
import zio.http.{ Version, Headers, Method, URL, Request, Body }
import zio.http.*
import is.clipperz.backend.Main
import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.bytesToHex
import is.clipperz.backend.functions.crypto.HashFunction
import java.nio.file.Path
import is.clipperz.backend.functions.FileSystem
import is.clipperz.backend.services.PRNG
import is.clipperz.backend.services.SessionManager
import is.clipperz.backend.services.UserArchive
import is.clipperz.backend.services.BlobArchive
import is.clipperz.backend.services.TollManager
import is.clipperz.backend.services.SrpManager

import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.services.OneTimeShareArchive

object BlobSpec extends ZIOSpecDefault:
  val app = Main.clipperzBackend
  val blobBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "blobs").nn
  val userBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "users").nn
  val oneTimeShareBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "one_time_share").nn

  val environment =
    PRNG.live ++
      SessionManager.live ++
      UserArchive.fs(userBasePath, 2, false) ++
      BlobArchive.fs(blobBasePath, 2, false) ++
      OneTimeShareArchive.fs(oneTimeShareBasePath, 2, false) ++
      ((UserArchive.fs(userBasePath, 2, false) ++ PRNG.live) >>> SrpManager.v6a()) ++
      (PRNG.live >>> TollManager.live)

  val testFile = new File(
    "src/test/resources/blobs/4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63.blob"
  )

  val validBlobData: Array[Byte] =
      Files
        .readAllBytes(Paths.get("src/test/resources/blobs/4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63.blob"))
        .nn
  val validBlobHash = HexString("4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63")

  val invalidBlobKey = "f9032dd04636e22b80db4c87513952154b05df9bc15c6951a5a73d810e1c5cae"
  val invalidBlobContent = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam ante massa, congue a sapien vel, efficitur facilisis eros. Mauris varius leo ut dolor malesuada, a pretium est scelerisque. Integer ut."

  val post = Request(
    url = URL(Root / "api" / "blobs"),
    method = Method.POST,
    headers = Headers.empty,
    body = Body.fromStream(Form(FormField.StreamingBinary(name = "blob", data = ZStream.fromChunk(Chunk.fromArray(invalidBlobContent.getBytes().nn)), filename = Some(invalidBlobKey.toString()), contentType = MediaType.any)).multipartBytes(Boundary("--???--"))), //500 -> "Cannot decode body as streaming multipart/form-data without a known boundary"
    // body = Body.fromMultipartForm(Form(FormField.StreamingBinary(name = "blob", data = ZStream.fromChunk(Chunk.fromArray(invalidBlobContent.getBytes().nn)), filename = Some(invalidBlobKey.toString()), contentType = MediaType.any)), Boundary("--???")), //400 -> fields are Text and not StreamingBinary
    version = Version.Http_1_1,
    remoteAddress = None
  )

  val delete = Request(
    url = URL(Root / "api" / "blobs"),
    method = Method.DELETE,
    headers = Headers.empty,
    body = Body.fromMultipartForm(Form(FormField.Binary(name = "blob", data = Chunk.fromArray(validBlobData), filename = Some(validBlobHash.toString()), contentType = MediaType.any)), Boundary("???")),
    version = Version.Http_1_1,
    remoteAddress = None
  )

  val deleteDifferentHashes = Request(
    url = URL(Root / "api" / "blobs"),
    method = Method.DELETE,
    headers = Headers.empty,
    body = Body.fromMultipartForm(Form.empty.append(FormField.binaryField(name = "blob", data = Chunk.fromArray(validBlobData), filename = Some("aaaaaa"), mediaType = MediaType.any)), Boundary("???")),
    version = Version.Http_1_1,
    remoteAddress = None
  )

  val invalidDelete = Request(
    url = URL(Root / "api" / "blobs"),
    method = Method.DELETE,
    headers = Headers.empty,
    body = Body.fromMultipartForm(Form.empty.append(FormField.binaryField(name = "blob", data = Chunk.fromArray(invalidBlobContent.getBytes().nn), filename = Some(validBlobHash.toString()), mediaType = MediaType.any)), Boundary("???")),
    version = Version.Http_1_1,
    remoteAddress = None
  )

  val get = Request(
    url = URL(Root / "api" / "blobs" / validBlobHash.toString()),
    method = Method.GET,
    headers = Headers.empty,
    body = Body.empty,
    version = Version.Http_1_1,
    remoteAddress = None
  )

  val postInvalid = Request(
    url = URL(Root / "api" / "blobs"),
    method = Method.POST,
    headers = Headers.empty,
    body = Body.fromMultipartForm(Form.empty.append(FormField.binaryField(name = "blob", data = Chunk.fromArray(invalidBlobContent.getBytes().nn), filename = None, mediaType = MediaType.any)), Boundary("???")),
    version = Version.Http_1_1,
    remoteAddress = None
  )

  val postEmpty = Request(
    url = URL(Root / "api" / "blobs"),
    method = Method.POST,
    headers = Headers.empty,
    body = Body.fromMultipartForm(Form.empty.append(FormField.binaryField(name = "blob", data = Chunk.fromArray("".getBytes().nn), filename = None, mediaType = MediaType.any)), Boundary("--XXX")),
    version = Version.Http_1_1,
    remoteAddress = None
  )

  def spec = suite("BlobApis")(
    // test("DELETE valid blob -> 200") {
    //   for {
    //     statusCodeDelete <- app.runZIO(delete).map(response => response.status.code)
    //   } yield assertTrue(statusCodeDelete == 200)
    // },
    // test("DELETE invalid blob -> 400") {
    //   for {
    //     statusCodeDelete <- app.runZIO(invalidDelete).map(response => response.status.code)
    //   } yield assertTrue(statusCodeDelete == 400)
    // },
    test("POST correct blob -> 200") {
      for {
        statusCode <- app.runZIO(post).map(response => response.status.code)
      } yield assertTrue(statusCode == 200)
    },
    // test("GET blob") {
    //   for {
    //     statusCode <- app.runZIO(get).map(response => response.status.code)
    //   } yield assertTrue(statusCode == 200)
    // },
    // test("POST invalid blob -> 400") {
    //   for {
    //     statusCode <- app.runZIO(postInvalid).map(response => response.status.code)
    //   } yield assertTrue(statusCode == 400)
    // },
    // test("POST empty blob -> 400") {
    //   for {
    //     statusCode <- app.runZIO(postEmpty).map(response => response.status.code)
    //   } yield assertTrue(statusCode == 400)
    // },
    // test("POST -> hash response") {
    //   for {
    //     body <- app.runZIO(post).flatMap(response => response.body.asString)
    //   } yield assertTrue(body == validBlobData.toString())
    // },
    // test("POST / GET -> 200") {
    //   for {
    //     postReseponse <- app.runZIO(post)
    //     code <- app.runZIO(get).map(_.status.code)
    //   } yield assertTrue(code == 200)
    // },
    // test("POST / GET -> response content") {
    //   for {
    //     postReseponse <- app.runZIO(post)
    //     hash <- app.runZIO(get).flatMap(response =>
    //       response
    //         .body.asStream
    //         .run(ZSink.digest(MessageDigest.getInstance("SHA-256").nn))
    //         .map((chunk: Chunk[Byte]) => HexString.bytesToHex(chunk.toArray))
    //     )
    //   } yield assertTrue(hash == validBlobHash)
    // },
    // test("POST / DELETE -> _, 200") {
    //   for {
    //     statusCode <- app.runZIO(post).flatMap(_ => app.runZIO(delete).map(response => response.status.code))
    //   } yield assertTrue(statusCode == 200)
    // },
    // test("POST / DELETE different hashes -> _, 400") {
    //   for {
    //     statusCode <- app.runZIO(post).flatMap(_ => app.runZIO(deleteDifferentHashes).map(response => response.status.code))
    //   } yield assertTrue(statusCode == 400)
    // },
    // test("POST / DELETE / GET -> 200, 200, 404") {
    //   for {
    //     statusCodePost <- app.runZIO(post).map(response => response.status.code)
    //     statusCodeDelete <- app.runZIO(delete).map(response => response.status.code)
    //     statusCodeGet <- app.runZIO(get).map(response => response.status.code)
    //   } yield assertTrue(statusCodePost == 200, statusCodeDelete == 200, statusCodeGet == 404)
    // },
  ).provideLayerShared(environment) @@
    TestAspect.sequential @@
    TestAspect.beforeAll(ZIO.succeed(FileSystem.deleteAllFiles(blobBasePath.toFile().nn))) @@
    TestAspect.afterAll(ZIO.succeed(FileSystem.deleteAllFiles(blobBasePath.toFile().nn)))
