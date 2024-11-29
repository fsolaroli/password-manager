package is.clipperz.backend.services

import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.bytesToHex
import is.clipperz.backend.functions.crypto.HashFunction
import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.Exceptions.*

// import java.io.{ File, FileNotFoundException, FileOutputStream, IOException }
import java.io.{ FileNotFoundException, IOException, FileOutputStream }
// import java.nio.charset.StandardCharsets
// import java.nio.file.Path
import zio.nio.file.{ Files, Path }
import java.security.MessageDigest

import zio.{ Chunk, Duration, RIO, ZIO, ZLayer, Task }
import zio.stream.{ ZStream, ZSink }
import zio.json.{ JsonDecoder, JsonEncoder, DeriveJsonDecoder, DeriveJsonEncoder }
import zio.nio.charset.Charset
import zio.telemetry.opentelemetry.tracing.Tracing
// import java.nio.file.attribute.FileAttribute

// ----------------------------------------------------------------------------

type BlobHash = HexString

// ----------------------------------------------------------------------------

trait BlobArchive:
    def getBlob             (hash: BlobHash): RIO[Tracing, (ZStream[Any, Throwable, Byte], Long)]
    def getBlobIdentifier   (hash: BlobHash): RIO[Tracing, HexString]
    def saveBlob            (hash: BlobHash, identifier: HexString, content:  ZStream[Any, Throwable, Byte]): RIO[Tracing, BlobHash]
    def saveBlob_path       (identifier: HexString, filename: String, hash: BlobHash, content: Path): RIO[Tracing, BlobHash]
    def deleteBlob          (hash: BlobHash, identifier: HexString): RIO[Tracing, Unit]

object BlobArchive:
    val WAIT_TIME = 10000

    case class FileSystemBlobArchive(keyBlobArchive: KeyBlobArchive, tmpDir: Path) extends BlobArchive:
        override def getBlob(hash: BlobHash): RIO[Tracing, (ZStream[Any, Throwable, Byte], Long)] =
            keyBlobArchive.getBlob(hash.toString)

        override def getBlobIdentifier(hash: BlobHash): RIO[Tracing, HexString] =
            keyBlobArchive
                .getMetadata(hash.toString)
                .flatMap(_.run(ZSink.collectAll[Byte]))
                // .map(_.toArray).map(new String(_, StandardCharsets.UTF_8))
                .flatMap(Charset.Standard.utf8.decodeChunk(_))  //  TODO: how are we messing with this data? Why aren't we going directly from byte[] to HexString 🤔
                .map(chunk => chunk.toArray.mkString)
                .map(HexString(_))
/*
        private def _saveBlob (hash: BlobHash, identifier: HexString, content: ZStream[Any, Throwable, Byte]): Task[BlobHash] =
            val tmpFile = File.createTempFile("pre", "suff", tmpDir.toFile())
            ZIO.scoped:
                content
                    .timeoutFail(new EmptyContentException)(Duration.fromMillis(WAIT_TIME))
                    .tapSink(ZSink.fromOutputStream(new FileOutputStream(tmpFile)))
                    .run(ZSink.digest(MessageDigest.getInstance("SHA-256").nn))
                    .map((chunk: Chunk[Byte]) => HexString.bytesToHex(chunk.toArray))
                .flatMap { hash_ =>
                    if (hash_ == hash)
                    then ZIO.scoped:
                        Charset.Standard.utf8.encodeString(identifier.toString())
                            .map(ZStream.fromChunk)
                            .flatMap(identifierStream =>
                                keyBlobArchive
                                    .saveBlobWithMetadata(hash.toString, ZStream.fromPath(tmpFile.nn.toPath().nn), identifierStream)
                                    .map(_ => tmpFile.nn.delete())
                                    .map(_ => hash)
                            )
                    else ZIO.fail(new BadRequestException(s"Hash of content does not match with hash in request"))
                }
                .catchSome:
                    case ex: FileNotFoundException =>
                        val str: String =
                            if ex.getMessage() == null
                            then "The temporary file or the blob could not be saved"
                            else ex.getMessage().nn
                        ZIO.fail(new NonWritableArchiveException(str))
                    case ex: BadRequestException => ZIO.fail(ex)
                    case ex: EmptyContentException => ZIO.fail(ex)
                    case ex => ZIO.fail(new NonWritableArchiveException(s"${ex}"))
*/

        private def _saveBlob_nio (hash: BlobHash, identifier: HexString, content: ZStream[Any, Throwable, Byte]): RIO[Tracing, BlobHash] =
           ZIO.scoped:
                Files.createTempFileInScoped(dir=tmpDir, suffix=".tmp", prefix=None, fileAttributes = Nil)
                // Files.createTempFileIn(
                //     dir = tmpDir,
                //     suffix = ".tmp",
                //     prefix = None,
                //     fileAttributes = Nil
                // )
                .flatMap { tmpFile => content
                    .timeoutFail(new EmptyContentException)(Duration.fromMillis(WAIT_TIME))
                    .tapSink(ZSink.fromOutputStream(new FileOutputStream(tmpFile.toFile)))
                    .run(ZSink.digest(MessageDigest.getInstance("SHA-256").nn))
                    .map((chunk: Chunk[Byte]) => HexString.bytesToHex(chunk.toArray))
                    .flatMap { hash_ =>
                        if (hash_ == hash)
                        // then ZIO.scoped:
                            Charset.Standard.utf8.encodeString(identifier.toString())
                            .map(ZStream.fromChunk)
                            .flatMap(identifierStream => 
                                Files.readAllBytes(tmpFile)
                                .map(ZStream.fromChunk)
                                //  TODO: here the file is copied from `tmp` to the final destination; we may opt to just **move** it - Giulio Cesare 29/02/2024
                                .flatMap(contentStream => keyBlobArchive
                                    .saveBlobWithMetadata(hash.toString, contentStream, identifierStream)
                                    // .flatMap(_ => Files.delete(tmpFile)) //  Not needed when the tmp file in 'scoped'
                                    .map(_ => hash)
                                )
                            )
                        else ZIO.fail(new BadRequestException(s"Hash of content does not match with hash field provided"))
                    }
                    .catchSome:
                        case ex: FileNotFoundException =>
                            val str: String =
                                if ex.getMessage() == null
                                then "The temporary file or the blob could not be saved"
                                else ex.getMessage().nn
                            ZIO.fail(new NonWritableArchiveException(str))
                        case ex: BadRequestException    => ZIO.fail(ex)
                        case ex: EmptyContentException  => ZIO.fail(ex)
                        case ex: Exception              => ZIO.fail(new NonWritableArchiveException(s"${ex}"))
                }

        override def saveBlob(hash: BlobHash, identifier: HexString, content: ZStream[Any, Throwable, Byte]): RIO[Tracing, BlobHash] =
            _saveBlob_nio(hash, identifier, content)

        override def saveBlob_path(identifier: HexString, filename: String, hash: BlobHash, content: Path): RIO[Tracing, BlobHash] =
            if HexString(filename) == hash
            then 
                Charset.Standard.utf8.encodeString(identifier.toString())
                .map(ZStream.fromChunk)
                .flatMap(identifierStream => keyBlobArchive.saveBlobWithMetadata_fromPath(filename, content, identifierStream))
                .map(_ => hash)
            else ZIO.fail(new BadRequestException(s"Hash of content does not match with hash field provided"))

        // override def commitSavedBlob     (hash: BlobHash, identifier: HexString): Task[BlobHash] =
        //     ???

        override def deleteBlob(hash: BlobHash, identifier: HexString): RIO[Tracing, Unit] =
            ZIO.scoped:
                this.getBlobIdentifier(hash)
                    .flatMap(storedIdentifier =>
                        if storedIdentifier == identifier
                        then keyBlobArchive.deleteBlob(hash.toString)
                        else ZIO.fail(new BadRequestException(s"Wrong blob identifier provided"))
                    )

  // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

    def initializeBlobArchive (baseTmpPath: Path): Task[Unit] =
        Files.exists(baseTmpPath)
        .zip(Files.isDirectory(baseTmpPath))
        .flatMap(checks => 
            val result = checks match {
                case (true, false)  => Files.delete(baseTmpPath) *> Files.createDirectories(baseTmpPath)
                case (true, true)   => ZIO.succeed(())
                case (false, _)     => Files.createDirectories(baseTmpPath)
            }
            result
        )

    def fs (
        basePath: Path,
        levels: Int,
        requireExistingPath: Boolean = true,
    ): ZLayer[Tracing, Throwable, BlobArchive] =
        val keyBlobArchive = KeyBlobArchive.FileSystemKeyBlobArchive(basePath, levels, requireExistingPath)
        val baseTmpPath: Path = basePath / "tmp"
        ZLayer.scoped(
            initializeBlobArchive(baseTmpPath)
            *>
            keyBlobArchive.map(FileSystemBlobArchive(_, baseTmpPath))
        )
