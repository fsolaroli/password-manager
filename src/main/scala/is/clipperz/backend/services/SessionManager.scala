package is.clipperz.backend.services

import is.clipperz.backend.Exceptions.*
import is.clipperz.backend.data.HexString.bytesToHex

import java.util.concurrent.TimeUnit

import scala.collection.immutable.HashMap
import scala.concurrent.duration.fromNow

import zio.{ Duration, Ref, ZIO, Layer, ZLayer, Tag, Task, UIO, durationInt }
import zio.cache.{ Cache, Lookup }
import zio.internal.stacktracer.Tracer
import zio.http.Request
import zio.http.codec.HeaderCodec
import zio.http.codec.HttpCodec

type SessionKey = String
type SessionContent = Map[String, String]

case class Session(val key: SessionKey, val content: SessionContent):
  def `+`(contentTuple: (String, String)): Session =
    Session(key, content + contentTuple)
  def apply(contentKey: String): Option[String] =
    content.get(contentKey)
  def isEmpty: Boolean =
    content.isEmpty

trait SessionManager:
  def getSession(sessionKey: String): Task[Session]
  def getSessionFromRequest(request: Request): Task[Session]
  def saveSession(content: Session): Task[SessionKey]
  def updateSession(request: Request, newC: String): Task[SessionKey] =
    getSessionFromRequest(request).map(_ + ("c", newC)).flatMap(saveSession(_))
  def verifySessionUser(c: String, session: Session): Boolean =
    session("c") match
      case Some(session_c) => session_c == c
      case None => false
  def deleteSession(request: Request): Task[Unit]

object SessionManager:
    val sessionKeyHeaderName = "clipperz-usersession-id"

    val sessionHeaderCodec: HeaderCodec[String] = HttpCodec.name[String](sessionKeyHeaderName)

    private def extractSessionKey(request: Request): Task[SessionKey] =
        ZIO
        .attempt(request.rawHeader(SessionManager.sessionKeyHeaderName).get)
        .mapError(_ => new NoSuchElementException("session header key not found when deleting session"))

    private def getSessionKey(prng: PRNG, request: Request): Task[SessionKey] =
        extractSessionKey(request)
        .catchAll(_ => prng.nextBytes(32).map(bytesToHex(_).toString()))

    case class ZioCacheSessionManager (prng: PRNG, sessions: Cache[String, Nothing, Ref[Session]]) extends SessionManager:
        
        private def refreshSessionTimeout(session: Session) =
            for {
                _       <- sessions.invalidate(session.key)
                ref     <- sessions.get(session.key)
                _       <- ref.set(session)
            } yield ()

        override def getSessionFromRequest(request: Request): Task[Session] =
            getSessionKey(prng, request).flatMap(getSession(_))

        override def getSession (key: String): Task[Session] =
            for {
                session <- sessions.get(key).flatMap(_.get)
                _       <- refreshSessionTimeout(session)
            } yield session

        override def saveSession(session: Session): Task[SessionKey] =
            refreshSessionTimeout(session).map(_ => session.key)

        override def deleteSession (request: Request): Task[Unit] =
            extractSessionKey(request)
            .flatMap (key => sessions.invalidate(key))

    def live(timeToLive: Duration = 10.minutes): ZLayer[PRNG, Throwable, SessionManager] =
        ZLayer.scoped(
            for {
                prng      <- ZIO.service[PRNG]
                sessions  <- Cache.make(capacity = 100, timeToLive = timeToLive, lookup = Lookup((key: SessionKey) => Ref.make(Session(key, HashMap.empty))))
            } yield ZioCacheSessionManager(prng, sessions)
        )
