package sculptor.xsd

import scala.xml._
import cats._, cats.data._, cats.implicits._

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

  private def foldChild[A](handlers: Eval[Seq[Tuple2[String, Op[A]]]]): Op[A] =
    a =>
      n =>
        withNode(n) {
          for {
            h <- ok(Map(handlers.value: _*))
            child <- xsdChild(n)
            res <- child.foldLeftM(a) { (a, n) =>
              h.get(n.label)
                .fold(error[A](s"Unknown element of type `${fullName(n)}`"))(
                  _(a)(n)
                )
            }
          } yield res
    }

  private def schema0[A](annotation: => Op[A],
                         simpleType: => Op[A],
                         complexType: => Op[A],
                         element: => Op[A],
                         attribute: => Op[A]): Op[A] =
    foldChild(
      Eval.later(
        Seq(
          "annotation" -> annotation,
          "simpleType" -> simpleType,
          "complexType" -> complexType,
          "element" -> element,
          "attribute" -> attribute
        )
      )
    )

  def schema[A](annotation: => Op[A],
                simpleType: => Op[A],
                complexType: => Op[A],
                element: => Op[A],
                attribute: => Op[A]): Op[A] =
    a =>
      n =>
        withNode(n) {
          for {
            ns <- findSchemaNs(n)
            _ <- updateSchemaNs(ns)
            l <- xsdLabel(n)
            res <- l match {
              case "schema" =>
                schema0(
                  annotation,
                  simpleType,
                  complexType,
                  element,
                  attribute
                )(a)(n)
              case l => error[A](s"`schema` tag was expected, found `$l`")
            }
          } yield res
    }

  def annotation[A](appinfo: => Op[A], documentation: => Op[A]): Op[A] =
    foldChild(
      Eval.later(Seq("appinfo" -> appinfo, "documentation" -> documentation))
    )

  def simpleType[A](annotation: => Op[A],
                    list: => Op[A],
                    restriction: => Op[A],
                    union: => Op[A]): Op[A] =
    foldChild(
      Eval.later(
        Seq(
          "annotation" -> annotation,
          "list" -> list,
          "restriction" -> restriction,
          "union" -> union
        )
      )
    )

  def list[A](annotation: => Op[A], simpleType: => Op[A]): Op[A] =
    foldChild(
      Eval.later(Seq("annotation" -> annotation, "simpleType" -> simpleType))
    )

  def restriction[A](annotation: => Op[A],
                     simpleType: => Op[A],
                     minExclusive: => Op[A],
                     minInclusive: => Op[A],
                     maxExclusive: => Op[A],
                     maxInclusive: => Op[A],
                     totalDigits: => Op[A],
                     fractionDigits: => Op[A],
                     length: => Op[A],
                     minLength: => Op[A],
                     maxLength: => Op[A],
                     enumeration: => Op[A],
                     whiteSpace: => Op[A],
                     pattern: => Op[A]): Op[A] =
    foldChild(
      Eval.later(
        Seq(
          "annotation" -> annotation,
          "simpleType" -> simpleType,
          "minExclusive" -> minExclusive,
          "minInclusive" -> minInclusive,
          "maxExclusive" -> maxExclusive,
          "maxInclusive" -> maxInclusive,
          "totalDigits" -> totalDigits,
          "fractionDigits" -> fractionDigits,
          "length" -> length,
          "minLength" -> minLength,
          "maxLength" -> maxLength,
          "enumeration" -> enumeration,
          "whiteSpace" -> whiteSpace,
          "pattern" -> pattern
        )
      )
    )

  def union[A](annotation: => Op[A], simpleType: => Op[A]): Op[A] =
    foldChild(
      Eval.later(Seq("annotation" -> annotation, "simpleType" -> simpleType))
    )

  def complexType[A](annotation: => Op[A],
                     simpleContent: => Op[A],
                     complexContent: => Op[A],
                     group: => Op[A],
                     all: => Op[A],
                     choice: => Op[A],
                     sequence: => Op[A],
                     attribute: => Op[A]): Op[A] =
    foldChild(
      Eval.later(
        Seq(
          "annotation" -> annotation,
          "simpleContent" -> simpleContent,
          "complexContent" -> complexContent,
          "group" -> group,
          "all" -> all,
          "choice" -> choice,
          "sequence" -> sequence,
          "attribute" -> attribute
        )
      )
    )

  def group[A](annotation: => Op[A],
               all: => Op[A],
               choice: => Op[A],
               sequence: => Op[A]): Op[A] =
    foldChild(
      Eval.later(
        Seq(
          "annotation" -> annotation,
          "all" -> all,
          "choice" -> choice,
          "sequence" -> sequence
        )
      )
    )

  def all[A](annotation: => Op[A], element: => Op[A]): Op[A] =
    foldChild(
      Eval.later(Seq("annotation" -> annotation, "element" -> element))
    )

  def choice[A](annotation: => Op[A],
                element: => Op[A],
                group: => Op[A],
                choice: => Op[A],
                sequence: => Op[A],
                any: => Op[A]): Op[A] =
    foldChild(
      Eval.later(
        Seq(
          "annotation" -> annotation,
          "element" -> element,
          "group" -> group,
          "choice" -> choice,
          "sequence" -> sequence,
          "any" -> any
        )
      )
    )

  def sequence[A](annotation: => Op[A],
                  element: => Op[A],
                  group: => Op[A],
                  choice: => Op[A],
                  sequence: => Op[A],
                  any: => Op[A]): Op[A] =
    foldChild(
      Eval.later(
        Seq(
          "annotation" -> annotation,
          "element" -> element,
          "group" -> group,
          "choice" -> choice,
          "sequence" -> sequence,
          "any" -> any
        )
      )
    )

  def any[A](annotation: => Op[A]): Op[A] =
    foldChild(Eval.later(Seq("annotation" -> annotation)))

  def simpleContent[A](annotation: => Op[A],
                       restriction: => Op[A],
                       extension: => Op[A]): Op[A] =
    foldChild(
      Eval.later(
        Seq(
          "annotation" -> annotation,
          "restriction" -> restriction,
          "extension" -> extension
        )
      )
    )

  def simpleContentRestriction[A](annotation: => Op[A],
                                  simpleType: => Op[A],
                                  minExclusive: => Op[A],
                                  minInclusive: => Op[A],
                                  maxExclusive: => Op[A],
                                  maxInclusive: => Op[A],
                                  totalDigits: => Op[A],
                                  fractionDigits: => Op[A],
                                  length: => Op[A],
                                  minLength: => Op[A],
                                  maxLength: => Op[A],
                                  enumeration: => Op[A],
                                  whiteSpace: => Op[A],
                                  pattern: => Op[A],
                                  attribute: => Op[A],
                                  attributeGroup: => Op[A],
                                  anyAttribute: => Op[A]): Op[A] =
    foldChild(
      Eval.later(
        Seq(
          "annotation" -> annotation,
          "simpleType" -> simpleType,
          "minExclusive" -> minExclusive,
          "minInclusive" -> minInclusive,
          "maxExclusive" -> maxExclusive,
          "maxInclusive" -> maxInclusive,
          "totalDigits" -> totalDigits,
          "fractionDigits" -> fractionDigits,
          "length" -> length,
          "minLength" -> minLength,
          "maxLength" -> maxLength,
          "enumeration" -> enumeration,
          "whiteSpace" -> whiteSpace,
          "pattern" -> pattern,
          "attribute" -> attribute,
          "attributeGroup" -> attributeGroup,
          "anyAttribute" -> anyAttribute
        )
      )
    )

  def simpleContentExtension[A](annotation: => Op[A],
                                attribute: => Op[A],
                                attributeGroup: => Op[A],
                                anyAttribute: => Op[A]): Op[A] =
    foldChild(
      Eval.later(
        Seq(
          "annotation" -> annotation,
          "attribute" -> attribute,
          "attributeGroup" -> attributeGroup,
          "anyAttribute" -> anyAttribute
        )
      )
    )

  def attribute[A](annotation: => Op[A], simpleType: => Op[A]): Op[A] =
    foldChild(
      Eval.later(Seq("annotation" -> annotation, "simpleType" -> simpleType))
    )

  def attributeGroup[A](annotation: => Op[A],
                        attribute: => Op[A],
                        attributeGroup: => Op[A],
                        anyAttribute: => Op[A]): Op[A] =
    foldChild(
      Eval.later(
        Seq(
          "annotation" -> annotation,
          "attribute" -> attribute,
          "attributeGroup" -> attributeGroup,
          "anyAttribute" -> anyAttribute
        )
      )
    )

  def anyAttribute[A](annotation: => Op[A]): Op[A] =
    foldChild(Eval.later(Seq("annotation" -> annotation)))

  def element[A](annotation: => Op[A],
                 simpleType: => Op[A],
                 complexType: => Op[A],
                 unique: => Op[A],
                 key: => Op[A],
                 keyref: => Op[A]): Op[A] =
    foldChild(
      Eval.later(
        Seq(
          "annotation" -> annotation,
          "simpleType" -> simpleType,
          "complexType" -> complexType,
          "unique" -> unique,
          "key" -> key,
          "keyref" -> keyref
        )
      )
    )

  def unique[A](annotation: => Op[A],
                selector: => Op[A],
                field: => Op[A]): Op[A] =
    foldChild(
      Eval.later(
        Seq(
          "annotation" -> annotation,
          "selector" -> selector,
          "field" -> field
        )
      )
    )

  def key[A](annotation: => Op[A],
             selector: => Op[A],
             field: => Op[A]): Op[A] =
    foldChild(
      Eval.later(
        Seq(
          "annotation" -> annotation,
          "selector" -> selector,
          "field" -> field
        )
      )
    )

  def keyref[A](annotation: => Op[A],
                selector: => Op[A],
                field: => Op[A]): Op[A] =
    foldChild(
      Eval.later(
        Seq(
          "annotation" -> annotation,
          "selector" -> selector,
          "field" -> field
        )
      )
    )

  def selector[A](annotation: => Op[A]): Op[A] =
    foldChild(Eval.later(Seq("annotation" -> annotation)))

  def field[A](annotation: => Op[A]): Op[A] =
    foldChild(Eval.later(Seq("annotation" -> annotation)))

  def nop[A]: Op[A] = a => _ => ok(a)
}
