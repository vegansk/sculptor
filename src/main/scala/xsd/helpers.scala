package sculptor.xsd

import sculptor.types._
import scala.util.Try
import scala.xml._
import cats.implicits._, cats.data._

private[xsd] object helpers {

  type Result[A] = Either[Throwable, A]

  final case class ParserState(ns: Option[String] = None,
                               path: xml.Path = xml.mkPath)

  type ParserStateS[A] = State[ParserState, A]
  type ResultS[A] = EitherT[ParserStateS, Throwable, A]

  def right[A](v: A): ResultS[A] = EitherT.rightT(v)
  def left[A](v: => Throwable): ResultS[A] = EitherT.leftT(v)
  def leftStr[A](s: => String): ResultS[A] = EitherT.leftT(new Exception(s))
  def liftS[A](s: ParserStateS[A]): ResultS[A] = EitherT.liftT(s)
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

  def prefixed(name: String): Result[(Option[String], String)] =
    name.split(":", 2).toList.splitAt(1) match {
      case (name :: _, Nil) => (None, name).pure[Result]
      case (prefix :: _, name :: _) => (Some(prefix), name).pure[Result]
      case _ => Left(new Exception("Can't make empty string prefixed"))
    }

  def parseNumber(v: String): Either[Throwable, Number] =
    Either.catchNonFatal(BigDecimal(v))

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
  def optElAttrAsNumber(name: String, attrName: String)(
    node: Node
  ): ResultS[Option[Number]] =
    OptionT(elO(name)(node))
      .semiflatMap(attr(attrName)(_))
      .semiflatMap(v => liftE(parseNumber(v)))
      .value

  def withComplexBody[A](whenUnknown: Node => A)(
    whenSequence: Node => A,
    whenChoice: Node => A,
    whenAll: Node => A
  ): Node => A = _ match {
    //TODO: Namespaces!!!
    case el if el.label === "sequence" => whenSequence(el)
    case el if el.label === "choice" => whenChoice(el)
    case el if el.label === "all" => whenAll(el)
    case el => whenUnknown(el)
  }

  def withComplexBodyO[A](whenSequence: Node => A,
                          whenChoice: Node => A,
                          whenAll: Node => A): Node => Option[A] =
    withComplexBody(_ => none[A])(
      whenSequence(_).some,
      whenChoice(_).some,
      whenAll(_).some
    )

  def withComplexBodyS[A](
    whenSequence: Node => ResultS[A],
    whenChoice: Node => ResultS[A],
    whenAll: Node => ResultS[A]
  )(node: Node): ResultS[A] =
    withComplexBody(
      el => leftStr[A](s"Unknown body type ${el.label} for complexType")
    )(whenSequence, whenChoice, whenAll)(node)

  def withComplexType[A](whenUnknown: => A)(
    whenSequence: Node => A,
    whenChoice: Node => A,
    whenAll: Node => A
  )(node: Node): A = {
    xml
      .childElems(node)
      .collectFirst(
        Function.unlift(withComplexBodyO(whenSequence, whenChoice, whenAll))
      )
      .getOrElse(whenUnknown)
  }

  def withComplexTypeS[A](
    whenSequence: Node => ResultS[A],
    whenChoice: Node => ResultS[A],
    whenAll: Node => ResultS[A]
  )(node: Node): ResultS[A] =
    withComplexType(leftStr[A]("Can't find the body of the complexType"))(
      whenSequence,
      whenChoice,
      whenAll
    )(node)
}
