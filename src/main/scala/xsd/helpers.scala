package sculptor.xsd

import sculptor._
import scala.util.Try
import scala.xml._
import cats._, cats.implicits._, cats.data._

object helpers {

  // These are used for compile pass
  sealed trait CType
  final case class LinkedType(`type`: types.Type) extends CType
  final case class UnlinkedType(name: String) extends CType
  final case class CRecord(name: Option[String], fields: Map[String, CType])
      extends CType
  final case class CModule(name: Option[String], types: List[CType])

  type Result[A] = Either[Throwable, A]

  final case class ParserState(ns: Option[String] = None,
                               path: xml.Path = xml.mkPath)

  type ParserStateS[A] = State[ParserState, A]
  type ResultS[A] = EitherT[ParserStateS, Throwable, A]

  def right[A](v: A): ResultS[A] = EitherT.rightT(v)
  def left(v: => Throwable): ResultS[Nothing] = EitherT.leftT(v)
  def liftS[A](s: ParserStateS[A]): ResultS[A] = EitherT.liftT(s)
  def liftE[A](e: Result[A]): ResultS[A] = EitherT(State.pure(e))

  type ParsedElement = (String, CType)

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

  def prefixed(name: String): Result[(Option[String], String)] =
    name.split(":", 2).toList.splitAt(1) match {
      case (name :: _, Nil) => (None, name).pure[Result]
      case (prefix :: _, name :: _) => (Some(prefix), name).pure[Result]
      case _ => Left(new Exception("Can't make empty string prefixed"))
    }

  def parseInt(v: String): Either[Throwable, Int] =
    Either.catchNonFatal(Integer.parseInt(v))

  def attr(name: String)(node: Node): ResultS[String] =
    liftE(xml.getAttr(name)(node))
  def attrO(name: String)(node: Node): ResultS[Option[String]] =
    right(xml.getAttrO(name)(node))
  def el(name: String)(node: Node): ResultS[Node] =
    getNs.flatMap(ns => liftE(xml.getByName(name, ns)(node)))
  def elO(name: String)(node: Node): ResultS[Option[Node]] =
    getNs.flatMap(ns => right(xml.getByNameO(name, ns)(node)))
  def els(name: String)(node: Node): ResultS[List[Node]] =
    getNs.flatMap(ns => right(xml.findByName(name, ns)(node)))
  def intAttr(name: String)(node: Node): ResultS[Int] =
    attr(name)(node).flatMap(v => liftE(Try(Integer.parseInt(v)).toEither))
  def optElAttrAsInt(name: String,
                     attrName: String)(node: Node): ResultS[Option[Int]] =
    OptionT(elO(name)(node))
      .semiflatMap(attr(attrName)(_))
      .semiflatMap(v => liftE(parseInt(v)))
      .value

}
