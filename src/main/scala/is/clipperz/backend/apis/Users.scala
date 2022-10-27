package is.clipperz.backend.apis

import zio.{ ZIO }
import zio.json.EncoderOps
import zio.stream.{ ZStream }
import zhttp.http.{ Http, Method, Response, Status }
import zhttp.http.* //TODO: fix How do you import `!!` and `/`?
import is.clipperz.backend.data.HexString
import is.clipperz.backend.exceptions.{
  NonWritableArchiveException, 
  NonReadableArchiveException, 
  FailedConversionException, 
  BadRequestException,
  ResourceConflictException,
  ResourceNotFoundException,
  ConflictualRequestException
}
import is.clipperz.backend.functions.{ fromStream }
import is.clipperz.backend.services.{
  BlobArchive,
  SessionManager,
  SignupData,
  UserArchive,
  UserCard
}
import is.clipperz.backend.Main.ClipperzHttpApp

val usersApi: ClipperzHttpApp = Http.collectZIO {
  case request @ Method.POST -> !! / "users" / c =>
    ZIO.service[UserArchive]
      .zip(ZIO.service[BlobArchive])
      .zip(ZIO.service[SessionManager])
      .zip(ZIO.succeed(request.bodyAsStream))
      .flatMap((userArchive, blobArchive, sessionManager, content) =>
        userArchive.getUser(HexString(c)).flatMap(optionalUser => optionalUser match
          case Some(_) => ZIO.fail(new ConflictualRequestException("User already exists"))
          case None    => ZIO.succeed(())
        ).flatMap(_ =>
          fromStream[SignupData](content)
          .flatMap(signupData => {
            if HexString(c) == signupData.user.c then
              ( userArchive.saveUser(signupData.user, false)
                <&> //Returns an effect that executes both this effect and the specified effect, in parallel, combining their results into a tuple. If either side fails, then the other side will be interrupted.
                blobArchive.saveBlob(signupData.indexCardReference, ZStream.fromIterable(signupData.indexCardContent.toByteArray))
                <&>
                ZIO.foreach(signupData.cards) { (reference, content) => blobArchive.saveBlob(reference, ZStream.fromIterable(content.toByteArray)) }
              ).parallelErrors.foldZIO(
                  err      => ZIO.fail(new Exception(s"${err}")),
                  result => ZIO.succeed(result)
              )                
            else  
              ZIO.fail(new BadRequestException("c in request path differs from c in request body "))
          })
        )
      )
      .map(results => Response.text(results._1.toString))
      .catchSome {
        case ex : ResourceConflictException => ZIO.succeed(Response(status = Status.Conflict))
        case ex : ConflictualRequestException => ZIO.succeed(Response(status = Status.Conflict))
        case ex : BadRequestException => { /* println(ex); */ ZIO.succeed(Response(status = Status.BadRequest)) }
        case ex : NonWritableArchiveException => { println(ex); ZIO.succeed(Response(status = Status.InternalServerError)) }
        case ex : FailedConversionException => { /* println(ex);  */ZIO.succeed(Response(status = Status.BadRequest)) }
        case ex => { println(ex); ZIO.fail(ex)}
      }

  case request @ Method.PUT -> !! / "users" / c =>
    ZIO.service[UserArchive]
      .zip(ZIO.service[BlobArchive])
      .zip(ZIO.service[SessionManager])
      .zip(ZIO.succeed(request.bodyAsStream))
      .flatMap((userArchive, blobArchive, sessionManager, content) =>
        sessionManager.verifySessionUser(c, request).flatMap(_ =>
          userArchive.getUser(HexString(c)).flatMap(optionalUser => optionalUser match
            case Some(u) => ZIO.succeed(u)
            case None    => ZIO.fail(new ResourceNotFoundException(s"user ${c} does not exist"))  
          )
        ).flatMap(_ =>
          fromStream[UserCard](content)
          .flatMap(user => {
            if HexString(c) == user.c then
              userArchive.saveUser(user, true).map(result => ZIO.succeed(result))
            else  
              ZIO.fail(new BadRequestException("c in request path differs from c in request body "))
          })
        )
      )
      .map(results => Response.text(results.toString))
      .catchSome {
        case ex : ResourceNotFoundException => ZIO.succeed(Response(status = Status.NotFound))
        case ex : BadRequestException => { /* println(ex); */ ZIO.succeed(Response(status = Status.BadRequest)) }
        case ex : NonWritableArchiveException => { println(ex); ZIO.succeed(Response(status = Status.InternalServerError)) }
        case ex : FailedConversionException => { /* println(ex);  */ZIO.succeed(Response(status = Status.BadRequest)) }
      }

  case request @ Method.GET -> !! / "users" / c =>
    ZIO.service[UserArchive]
      .zip(ZIO.service[SessionManager])
      .flatMap((userArchive, sessionManager) =>
        sessionManager.verifySessionUser(c, request)
        .flatMap(_ => userArchive.getUser(HexString(c)))
      )
      .map(maybeCard => 
        maybeCard match
          case None => Response(status = Status.NotFound)
          case Some(card) => Response.json(card.toJson)
      )
      .catchSome {
        case ex : NonReadableArchiveException => { println(ex); ZIO.succeed(Response(status = Status.InternalServerError)) }
        case ex : BadRequestException => { /* println(ex); */ ZIO.succeed(Response(status = Status.BadRequest)) }
      }

  case request @ Method.DELETE -> !! / "users" / c =>
    ZIO.service[UserArchive]
      .zip(ZIO.service[SessionManager])
      .zip(ZIO.succeed(request.bodyAsStream))
      .flatMap((userArchive, sessionManager, content) =>
        sessionManager
          .verifySessionUser(c, request)
          .flatMap(_ =>
            fromStream[UserCard](content)
              .flatMap(userCard => userArchive.deleteUser(userCard))
              .map(b => if b then Response.text(c) else Response(status = Status.NotFound))
          )
      )
      .catchSome {
        case ex : NonWritableArchiveException => { println(ex); ZIO.succeed(Response(status = Status.InternalServerError)) }
        case ex : BadRequestException => ZIO.succeed(Response(status = Status.BadRequest))
        case ex : ResourceNotFoundException => ZIO.succeed(Response(status = Status.NotFound))
      }
}
