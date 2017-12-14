package sculptor.xsd

import scala.xml._
import cats.data._

private[xsd] object helpers {

  type Result[A] = Either[Throwable, A]

  final case class ParserState(ns: Option[String] = None,
                               path: xml.Path = xml.mkPath)

  type ParserStateS[A] = State[ParserState, A]
  type ResultS[A] = EitherT[ParserStateS, Throwable, A]

  def right[A](v: A): ResultS[A] = EitherT.rightT(v)
  def left[A](v: => Throwable): ResultS[A] = EitherT.leftT(v)
  def leftStr[A](s: => String): ResultS[A] = EitherT.leftT(new Exception(s))
  def liftS[A](s: ParserStateS[A]): ResultS[A] = EitherT.liftF(s)
  def liftE[A](e: Result[A]): ResultS[A] = EitherT(State.pure(e))

  def getNs: ResultS[Option[String]] = liftS(State.get[ParserState].map(_.ns))
  def updateNs(ns: Option[String]): ResultS[Unit] =
    liftS(State.modify[ParserState](_.copy(ns = ns)))
  def pushNode(n: Node): ResultS[Unit] =
    liftS(State.modify[ParserState](s => s.copy(path = xml.push(s.path, n))))
  def popNode: ResultS[Unit] =
    liftS(State.modify[ParserState](s => s.copy(path = xml.pop(s.path))))
  def withNode[A](n: Node)(body: => ResultS[A]): ResultS[A] = {
    for {
      _ <- pushNode(n)
      r <- body
      _ <- popNode
    } yield r
  }
}
