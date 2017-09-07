package sculptor.xsd

import scala.xml._
import cats.data._, cats.implicits._

object fold {

  final case class FoldState(schemaNs: Option[String] = None,
                             path: xml.Path = xml.mkPath)

  type FS[A] = State[FoldState, A]

  type Result[A] = EitherT[FS, String, A]

  type Op[A] = A => Node => Result[A]

  def ok[A](a: A): Result[A] = EitherT.rightT(a)

  def error[A](err: String): Result[A] = EitherT.leftT(err)

  def liftS[A](s: FS[A]): Result[A] = EitherT.liftT(s)

  def getSchemaNs: Result[Option[String]] =
    liftS(State.get[FoldState].map(_.schemaNs))

  private def updateSchemaNs(ns: Option[String]): Result[Unit] =
    liftS(State.modify[FoldState](_.copy(schemaNs = ns)))

  private def pushNode(n: Node): Result[Unit] =
    liftS(State.modify[FoldState](s => s.copy(path = xml.push(s.path, n))))

  private def popNode: Result[Unit] =
    liftS(State.modify[FoldState](s => s.copy(path = xml.pop(s.path))))

  private def withNode[A](n: Node)(body: => Result[A]): Result[A] = {
    for {
      _ <- pushNode(n)
      r <- body
      _ <- popNode
    } yield r
  }

  private val XSD_NAMESPACE = "http://www.w3.org/2001/XMLSchema"

  private def findSchemaNs(xsd: Node): Result[Option[String]] =
    ok(xml.getPrefix(XSD_NAMESPACE)(xsd))

  private def fullName(n: Node): String = {
    val sb = new StringBuilder()
    n.nameToString(sb).toString
  }

  private def xsdLabel(n: Node): Result[String] =
    for {
      ns <- getSchemaNs
      _ <- if (Option(n.prefix) =!= ns)
        error(
          s"Element's prefix `${fullName(n)}` doesn't match the xml schema prefix `${ns.getOrElse("")}`"
        )
      else ok(())
    } yield n.label

  private def xsdChild(n: Node): Result[List[Node]] =
    for {
      ns <- getSchemaNs
      res <- ok(n.child.filter(ch => Option(ch.prefix) === ns).toList)
    } yield res

  private def foldChild[A](handlers: Tuple2[String, Op[A]]*): Op[A] =
    a =>
      n =>
        withNode(n) {
          for {
            h <- ok(Map(handlers: _*))
            child <- xsdChild(n)
            res <- child.foldLeftM(a) { (a, n) =>
              h.get(n.label)
                .fold(error[A](s"Unknown element of type `${fullName(n)}`"))(
                  _(a)(n)
                )
            }
          } yield res
    }

  private def schema0[A](annotation: Op[A],
                         simpleType: Op[A],
                         complexType: Op[A],
                         element: Op[A]): Op[A] =
    foldChild(
      "annotation" -> annotation,
      "simpleType" -> simpleType,
      "complexType" -> complexType,
      "element" -> element
    )

  def schema[A](annotation: Op[A],
                simpleType: Op[A],
                complexType: Op[A],
                element: Op[A]): Op[A] =
    a =>
      n =>
        withNode(n) {
          for {
            ns <- findSchemaNs(n)
            _ <- updateSchemaNs(ns)
            l <- xsdLabel(n)
            res <- l match {
              case "schema" =>
                schema0(annotation, simpleType, complexType, element)(a)(n)
              case l => error[A](s"`schema` tag was expected, found `$l`")
            }
          } yield res
    }

  def annotation[A](appinfo: Op[A], documentation: Op[A]): Op[A] =
    foldChild("appinfo" -> appinfo, "documentation" -> documentation)

  def simpleType[A]: Op[A] = ???

  def complexType[A]: Op[A] = ???

  def element[A]: Op[A] = ???
}
