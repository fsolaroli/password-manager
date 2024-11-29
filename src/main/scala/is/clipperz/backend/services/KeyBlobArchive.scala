package is.clipperz.backend.services

import is.clipperz.backend.Exceptions.*
import is.clipperz.backend.middleware.scheduledFileSystemMetricsCollection

import java.io.{ FileNotFoundException, FileOutputStream }
// import java.io.{ File, FileNotFoundException }
// import java.nio.file.{ Files, Path }
import zio.nio.file.{ Files, Path }

import zio.{ Duration, Task, RIO, ZIO }
import zio.stream.{ ZSink, ZStream }
import zio.http.codec.HttpCodec.Metadata
import zio.http.Header.ContentType
import zio.nio.file.Files.Attribute
import zio.nio.file.Files.Attributes
import zio.telemetry.opentelemetry.tracing.Tracing


// ============================================================================

type Key = String

trait KeyBlobArchive:
    def saveBlob             (key: Key, content:  ZStream[Any, Throwable, Byte]): RIO[Tracing, Unit]
    def saveBlobWithMetadata (key: Key, content:  ZStream[Any, Throwable, Byte], metadata: ZStream[Any, Throwable, Byte]): RIO[Tracing, Unit]
    def saveBlobWithMetadata_fromPath (key: Key, content:  Path, metadata: ZStream[Any, Throwable, Byte]): RIO[Tracing, Unit]
    def getBlob              (key: Key): RIO[Tracing, (ZStream[Any, Throwable, Byte], Long)]
    def getMetadata          (key: Key): RIO[Tracing, ZStream[Any, Throwable, Byte]]
    def deleteBlob           (key: Key): RIO[Tracing, Unit]

object KeyBlobArchive:
    val WAIT_TIME = 10000

    enum ContentType(val value: String):
        case Blob       extends ContentType("blob")
        case Metadata   extends ContentType("metadata")

    // def pathForContentType (path: Path, contentType: ContentType): Path = path.resolveSibling(Path(path.filename.toFile.getName().nn + "." + contentType.value))
    def pathForContentType (filename: Key, path: Path, contentType: ContentType): Path = path / s"${filename}.${contentType.value}"

    class FileSystemKeyBlobArchive private (basePath: Path, levels: Int) extends KeyBlobArchive:

        private def getContent (key: Key, contentType: ContentType): RIO[Tracing, (ZStream[Any, Throwable, Byte], Long)] =
            ZIO.serviceWithZIO[Tracing](tracing => tracing.span("getContent") {
                getBlobPath(key, false)
                .map(path => pathForContentType(key, path, contentType))
                .flatMap(path =>
                    Files.exists(path)
                    .flatMap(exists => exists match {
                        // case true   => ZStream.fromPath(path)
                        case true   => (Files.readAllBytes(path).map(ZStream.fromChunk)).zip(Files.size(path))
                        case false  => ZIO.fail(new ResourceNotFoundException("Referenced blob does not exists"))
                    })
                )
                // .getOrElse(ZIO.fail(new ResourceNotFoundException("Blob metadata not found")))
            })

        override def getBlob (key: Key): RIO[Tracing, (ZStream[Any, Throwable, Byte], Long)] = 
            ZIO.serviceWithZIO[Tracing](tracing => tracing.span("getBlob") {
                getContent(key, ContentType.Blob)
            })

        override def getMetadata(key: Key): RIO[Tracing, ZStream[Any, Throwable, Byte]] = 
            ZIO.serviceWithZIO[Tracing](tracing => tracing.span("getMetadata") {
                getContent(key, ContentType.Metadata).map(_._1)
            })

        private def saveData (key: Key, contentType: ContentType, content: ZStream[Any, Throwable, Byte]): RIO[Tracing, Unit] = 
            ZIO.serviceWithZIO[Tracing](tracing => tracing.span("saveData") {
                getBlobPath(key, true)
                .map(path => pathForContentType(key, path, contentType))
                .mapError(_ => new NonWritableArchiveException("Could not create blob file"))
                .flatMap(path => content
                    .timeoutFail(new EmptyContentException)(Duration.fromMillis(WAIT_TIME))
                    // .run(ZSink.fromPath(path))
                    .run(ZSink.fromOutputStream(new FileOutputStream(path.toFile)))
                    .map(_ => ())
                )
                .catchSome:
                    case ex: EmptyContentException => ZIO.fail(ex)
                    case ex: NonReadableArchiveException => ZIO.fail(ex)
                    case ex => ZIO.fail(new NonWritableArchiveException(s"${ex}"))
            })

        private def moveFile (key: Key, content: Path): RIO[Tracing, Unit] =
            ZIO.serviceWithZIO[Tracing](tracing => tracing.span("moveFile") {
                getBlobPath(key, true)
                .map(path => pathForContentType(key, path, ContentType.Blob))
                .flatMap(path => Files.move(content, path))
            })

        private  def saveMetadata (key: Key, metadata: ZStream[Any, Throwable, Byte]): RIO[Tracing, Unit]   =
            ZIO.serviceWithZIO[Tracing](tracing => tracing.span("saveMetadata") {
                saveData(key, ContentType.Metadata, metadata)
            })

        override def saveBlob     (key: Key, content:  ZStream[Any, Throwable, Byte]): RIO[Tracing, Unit]   =
            ZIO.serviceWithZIO[Tracing](tracing => tracing.span("saveBlob") { 
                saveData(key, ContentType.Blob,     content)
            })

        override def saveBlobWithMetadata (key: Key, content: ZStream[Any, Throwable, Byte], metadata: ZStream[Any, Throwable, Byte]): RIO[Tracing, Unit] =
            ZIO.serviceWithZIO[Tracing](tracing => tracing.span("saveBlobWithMetadata") {
                saveBlob(key, content) <&> saveMetadata(key, metadata)
            })

        override def saveBlobWithMetadata_fromPath (key: Key, content:  Path, metadata: ZStream[Any, Throwable, Byte]): RIO[Tracing, Unit] =
            ZIO.serviceWithZIO[Tracing](tracing => tracing.span("saveBlobWithMetadata_fromPath") {
                moveFile(key, content) <&> saveMetadata(key, metadata)
            })

        override def deleteBlob (key: Key): RIO[Tracing, Unit] =
            ZIO.serviceWithZIO[Tracing](tracing => tracing.span("saveMetadata") {
                getBlobPath(key, false)
                    .flatMap(path =>
                        Files.deleteIfExists(pathForContentType(key, path, ContentType.Blob))
                        <&>
                        Files.deleteIfExists(pathForContentType(key, path, ContentType.Metadata))
                    )
                // TODO: delete empty folder?
                .foldZIO(err => ZIO.fail(new NonWritableArchiveException(err.toString())), _ => ZIO.succeed(()))
            })

        private def computeBlobPath (key: Key): Path =
            val piecesLength: Int = key.length / levels
            // val pieces: IndexedSeq[String | Null] =
            val pieces: IndexedSeq[String] =
                for (i <- 0 to levels - 1)
                yield key.substring(i * piecesLength, i * piecesLength + piecesLength).nn
            // val subPathString: String = pieces.mkString("/")
            basePath / pieces.mkString("/")

        private def getBlobPath (key: Key, createFolders: Boolean): RIO[Tracing, Path] =
            val path: Path = computeBlobPath(key)
            ZIO.serviceWithZIO[Tracing](tracing => tracing.span("getBlobPath") {
                Files.exists(path)
                .zip(Files.isDirectory(path))
                // .tap((exists, isDirectory) => ZIO.log(s"GET BLOB PATH: ${key} => ${exists} - ${isDirectory} - ${createFolders}"))
                .flatMap((exists, isDirectory) => (exists, isDirectory, createFolders) match {
                    case (true,  true,  _    ) => ZIO.succeed(path)
                    case (true,  false, _    ) => ZIO.fail(new ResourceNotFoundException(s"Referenced blob does not exists"))
                    case (false, _,     false) => ZIO.fail(new ResourceNotFoundException(s"Referenced blob does not exists"))
                    case (false, _,     true ) => Files.createDirectories(path) *> ZIO.succeed(path)
                })
            })

    object FileSystemKeyBlobArchive:
        def apply (
            basePath: Path,
            levels: Int,
            requireExistingPath: Boolean = true,
        ): RIO[Tracing, FileSystemKeyBlobArchive] =
            Files.exists(basePath)
            .zip(Files.isDirectory(basePath))
            .flatMap((exists, isDirectory) => {
                // println(s"===> FileSystemKeyBlogArchive: path: ${basePath}, exists: ${exists}, ${isDirectory}, requireExistingPath: ${requireExistingPath} => ${((exists && isDirectory) || !requireExistingPath)}")
                ((exists, isDirectory, requireExistingPath) match {
                     case (true,  true,  _    )  => ZIO.succeed(())
                     case (true,  false, false)  => Files.deleteRecursive(basePath) *> Files.createDirectories(basePath) *> ZIO.succeed(())
                     case (false, _,     false)  => Files.createDirectories(basePath) *> ZIO.succeed(())
                     case (true,  false, true )  => ZIO.fail(new Exception(s"base folder file already exists, but is not a folder: ${basePath}"))
                     case (false, _,     true )  => ZIO.fail(new Exception(s"base folder does not exists: ${basePath}"))
                })
                *>  scheduledFileSystemMetricsCollection(basePath).forkDaemon
                *>  ZIO.succeed(new FileSystemKeyBlobArchive(basePath, levels))
            })