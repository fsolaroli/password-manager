package is.clipperz.backend

import zio.schema.Schema
import zio.schema.DeriveSchema

object Exceptions:
    case class BadRequestException(error: String) extends Exception(error):
        def this(message: String, cause: Throwable) =
            this(message)
            initCause(cause)
    
    object BadRequestException:
        implicit val schema: Schema[BadRequestException] = DeriveSchema.gen

    class ConflictualRequestException(error: String) extends Exception(error):
        def this(message: String, cause: Throwable) =
            this(message)
            initCause(cause)

    class EmptyContentException(error: String) extends Exception(error):
        def this(message: String, cause: Throwable) =
            this(message)
            initCause(cause)

        def this() =
            this("Content is not present")

    class FailedConversionException(error: String) extends Exception(error):
        def this(message: String, cause: Throwable) =
            this(message)
            initCause(cause)

    class NonReadableArchiveException(error: String) extends Exception(error):
        def this(message: String, cause: Throwable) =
            this(message)
            initCause(cause)

    class NonWritableArchiveException(error: String) extends Exception(error):
        def this(message: String, cause: Throwable) =
            this(message)
            initCause(cause)

    class ResourceConflictException(error: String) extends Exception(error):
        def this(message: String, cause: Throwable) =
            this(message)
            initCause(cause)

    class ResourceExpiredException(error: String) extends Exception(error):
        def this(message: String, cause: Throwable) =
            this(message)
            initCause(cause)

    case class ResourceNotFoundException(error: String) extends Exception(error):
        def this(message: String, cause: Throwable) =
            this(message)
            initCause(cause)
            
    object ResourceNotFoundException:
        implicit val schema: Schema[ResourceNotFoundException] = DeriveSchema.gen
