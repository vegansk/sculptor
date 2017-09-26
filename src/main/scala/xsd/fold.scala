package sculptor.xsd

import scala.xml._
import cats._, cats.data._, cats.implicits._

object fold {

  sealed trait FormDefault
  case object Qualified extends FormDefault
  case object Unqualified extends FormDefault

  implicit val formDefaultEqInstance: Eq[FormDefault] = new Eq[FormDefault] {
    @SuppressWarnings(Array("org.wartremover.warts.Equals"))
    def eqv(a: FormDefault, b: FormDefault) = a == b
  }

  final case class FoldState(schemaNs: Option[String] = None,
                             attributeFormDefault: FormDefault = Unqualified,
                             elementFormDefault: FormDefault = Unqualified,
                             path: xml.Path = xml.mkPath)

  type FS[A] = State[FoldState, A]

  type Result[A] = EitherT[FS, String, A]

  type Op[A] = A => Node => Result[A]

  type AttrOp[A] = A => String => Result[A]

  private type Handlers[F[_], A] = Eval[Seq[Tuple2[String, F[A]]]]

  def ok[A](a: A): Result[A] = EitherT.rightT(a)

  def error[A](err: String): Result[A] = EitherT.leftT(err)

  def liftS[A](s: FS[A]): Result[A] = EitherT.liftT(s)

  def getSchemaNs: Result[Option[String]] =
    liftS(State.get[FoldState].map(_.schemaNs))

  private def updateSchemaNs(ns: Option[String]): Result[Unit] =
    liftS(State.modify[FoldState](_.copy(schemaNs = ns)))

  def getAttributeFormDefault: Result[FormDefault] =
    liftS(State.get[FoldState].map(_.attributeFormDefault))

  private def updateAttributeFormDefault(a: FormDefault): Result[Unit] =
    liftS(State.modify[FoldState](_.copy(attributeFormDefault = a)))

  def getElementFormDefault: Result[FormDefault] =
    liftS(State.get[FoldState].map(_.elementFormDefault))

  private def updateElementFormDefault(e: FormDefault): Result[Unit] =
    liftS(State.modify[FoldState](_.copy(elementFormDefault = e)))

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

  private def findFormDefault(
    attrName: String
  )(xsd: Node): Result[Option[FormDefault]] =
    xsd.attribute(attrName).fold[Result[Option[FormDefault]]](ok(None)) {
      _.text match {
        case "qualified" => ok(Some(Qualified))
        case "unqualified" => ok(Some(Unqualified))
        case v => error(s"Unknown value `$v` of attrubute `$attrName`")
      }
    }

  private val findAttributeFormDefault = findFormDefault(
    "attributeFormDefault"
  ) _

  private val findElementFormDefault = findFormDefault("elementFormDefault") _

  private def fullName(n: Node): String = {
    val sb = new StringBuilder()
    n.nameToString(sb).toString
  }

  private val filterNs =
    (schemaNs: Option[String], ns: Option[String], default: FormDefault) =>
      ns match {
        case None if default === Qualified => true
        case ns if ns === schemaNs => true
        case _ => false
    }

  private def xsdLabel(n: Node): Result[String] =
    for {
      ns <- getSchemaNs
      elementFormDefault <- getElementFormDefault
      _ <- if (!filterNs(ns, Option(n.prefix), elementFormDefault))
        error(
          s"Element's prefix `${fullName(n)}` doesn't match the xml schema prefix `${ns.getOrElse("")}`"
        )
      else ok(())
    } yield n.label

  private def xsdChild(n: Node): Result[List[Node]] =
    for {
      ns <- getSchemaNs
      elementFormDefault <- getElementFormDefault
      res <- ok(
        n.child.toList
          .filter(ch => filterNs(ns, Option(ch.prefix), elementFormDefault))
      )
    } yield res

  def composeOps[A](x: Op[A])(y: Op[A]): Op[A] =
    a =>
      n =>
        for {
          a1 <- x(a)(n)
          a2 <- y(a1)(n)
        } yield a2

  private def foldChild[A](handlers: Handlers[Op, A]): Op[A] =
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

  private def xsdAttributes(n: Node): Result[List[Attribute]] =
    for {
      ns <- getSchemaNs
      attributeFormDefault <- getAttributeFormDefault
      res <- ok(
        n.attributes.toList
          .map {
            _ match {
              case x: Attribute => List(x)
              case _ => Nil
            }
          }
          .flatten
          .filter(a => filterNs(ns, Option(a.pre), attributeFormDefault))
      )
    } yield res

  private def foldAttributes[A](handlers: Handlers[AttrOp, A]): Op[A] =
    a =>
      n =>
        withNode(n) {
          for {
            h <- ok(Map(handlers.value: _*))
            attrs <- xsdAttributes(n)
            res <- attrs.foldLeftM(a) { (a, attr) =>
              h.get(attr.key)
                .fold(error[A](s"Unknown attribute `${attr.prefixedKey}`"))(
                  _(a)(attr.value.text)
                )
            }
          } yield res
    }

  private def schema0[A](id: => AttrOp[A],
                         targetNamespace: => AttrOp[A],
                         annotation: => Op[A],
                         simpleType: => Op[A],
                         complexType: => Op[A],
                         element: => Op[A],
                         attribute: => Op[A]): Op[A] =
    composeOps(
      foldAttributes(
        Eval.later(Seq("id" -> id, "targetNamespace" -> targetNamespace))
      )
    )(
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
    )

  def schema[A](id: => AttrOp[A] = attrNop[A],
                targetNamespace: => AttrOp[A] = attrNop[A],
                annotation: => Op[A] = nop[A],
                simpleType: => Op[A] = nop[A],
                complexType: => Op[A] = nop[A],
                element: => Op[A] = nop[A],
                attribute: => Op[A] = nop[A]): Op[A] =
    a =>
      n =>
        withNode(n) {
          for {
            _ <- findSchemaNs(n).flatMap(updateSchemaNs)
            _ <- findAttributeFormDefault(n).flatMap(
              _.fold(ok(()))(v => updateAttributeFormDefault(v))
            )
            _ <- findElementFormDefault(n).flatMap(
              _.fold(ok(()))(v => updateElementFormDefault(v))
            )
            l <- xsdLabel(n)
            res <- l match {
              case "schema" =>
                schema0(
                  id,
                  targetNamespace,
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

  def annotation[A](id: => AttrOp[A] = attrNop[A],
                    appinfo: => Op[A] = nop[A],
                    documentation: => Op[A] = nop[A]): Op[A] =
    composeOps(foldAttributes(Eval.later(Seq("id" -> id))))(
      foldChild(
        Eval.later(Seq("appinfo" -> appinfo, "documentation" -> documentation))
      )
    )

  def simpleType[A](id: => AttrOp[A] = attrNop[A],
                    `final`: => AttrOp[A] = attrNop[A],
                    name: => AttrOp[A] = attrNop[A],
                    annotation: => Op[A] = nop[A],
                    list: => Op[A] = nop[A],
                    restriction: => Op[A] = nop[A],
                    union: => Op[A] = nop[A]): Op[A] =
    composeOps(
      foldAttributes(
        Eval.later(Seq("final" -> `final`, "id" -> id, "name" -> name))
      )
    )(
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
    )

  def list[A](id: => AttrOp[A] = attrNop[A],
              itemType: => AttrOp[A] = attrNop[A],
              annotation: => Op[A] = nop[A],
              simpleType: => Op[A] = nop[A]): Op[A] =
    composeOps(
      foldAttributes(Eval.later(Seq("id" -> id, "itemType" -> itemType)))
    )(
      foldChild(
        Eval.later(Seq("annotation" -> annotation, "simpleType" -> simpleType))
      )
    )

  def simpleTypeRestriction[A](id: => AttrOp[A] = attrNop[A],
                               base: => AttrOp[A] = attrNop[A],
                               annotation: => Op[A] = nop[A],
                               simpleType: => Op[A] = nop[A],
                               minExclusive: => Op[A] = nop[A],
                               minInclusive: => Op[A] = nop[A],
                               maxExclusive: => Op[A] = nop[A],
                               maxInclusive: => Op[A] = nop[A],
                               totalDigits: => Op[A] = nop[A],
                               fractionDigits: => Op[A] = nop[A],
                               length: => Op[A] = nop[A],
                               minLength: => Op[A] = nop[A],
                               maxLength: => Op[A] = nop[A],
                               enumeration: => Op[A] = nop[A],
                               whiteSpace: => Op[A] = nop[A],
                               pattern: => Op[A] = nop[A]): Op[A] =
    composeOps(foldAttributes(Eval.later(Seq("id" -> id, "base" -> base))))(
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
    )

  def union[A](id: => AttrOp[A] = attrNop[A],
               memberTypes: => AttrOp[A] = attrNop[A],
               annotation: => Op[A] = nop[A],
               simpleType: => Op[A] = nop[A]): Op[A] =
    composeOps(
      foldAttributes(Eval.later(Seq("id" -> id, "memberTypes" -> memberTypes)))
    )(
      foldChild(
        Eval.later(Seq("annotation" -> annotation, "simpleType" -> simpleType))
      )
    )

  def complexType[A](id: => AttrOp[A] = attrNop[A],
                     `abstract`: => AttrOp[A] = attrNop[A],
                     block: => AttrOp[A] = attrNop[A],
                     `final`: => AttrOp[A] = attrNop[A],
                     mixed: => AttrOp[A] = attrNop[A],
                     name: => AttrOp[A] = attrNop[A],
                     annotation: => Op[A] = nop[A],
                     simpleContent: => Op[A] = nop[A],
                     complexContent: => Op[A] = nop[A],
                     group: => Op[A] = nop[A],
                     all: => Op[A] = nop[A],
                     choice: => Op[A] = nop[A],
                     sequence: => Op[A] = nop[A],
                     attribute: => Op[A] = nop[A]): Op[A] =
    composeOps(
      foldAttributes(
        Eval.later(
          Seq(
            "id" -> id,
            "abstract" -> `abstract`,
            "block" -> block,
            "final" -> `final`,
            "mixed" -> mixed,
            "name" -> name
          )
        )
      )
    )(
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
    )

  def complexContent[A](id: => AttrOp[A] = attrNop[A],
                        mixed: => AttrOp[A] = attrNop[A],
                        annotation: => Op[A] = nop[A],
                        restriction: => Op[A] = nop[A],
                        extension: => Op[A] = nop[A]): Op[A] =
    composeOps(foldAttributes(Eval.later(Seq("id" -> id, "mixed" -> mixed))))(
      foldChild(
        Eval.later(
          Seq(
            "annotation" -> annotation,
            "restriction" -> restriction,
            "extension" -> extension
          )
        )
      )
    )

  def complexContentRestriction[A](id: => AttrOp[A] = attrNop[A],
                                   base: => AttrOp[A] = attrNop[A],
                                   annotation: => Op[A] = nop[A],
                                   group: => Op[A] = nop[A],
                                   all: => Op[A] = nop[A],
                                   choice: => Op[A] = nop[A],
                                   sequence: => Op[A] = nop[A],
                                   attribute: => Op[A] = nop[A],
                                   attributeGroup: => Op[A] = nop[A],
                                   anyAttribute: => Op[A] = nop[A]): Op[A] =
    composeOps(foldAttributes(Eval.later(Seq("id" -> id, "base" -> base))))(
      foldChild(
        Eval.later(
          Seq(
            "annotation" -> annotation,
            "group" -> group,
            "all" -> all,
            "choice" -> choice,
            "sequence" -> sequence,
            "attribute" -> attribute,
            "attributeGroup" -> attributeGroup,
            "anyAttribute" -> anyAttribute
          )
        )
      )
    )

  def complexContentExtension[A](id: => AttrOp[A] = attrNop[A],
                                 base: => AttrOp[A] = attrNop[A],
                                 annotation: => Op[A] = nop[A],
                                 group: => Op[A] = nop[A],
                                 all: => Op[A] = nop[A],
                                 choice: => Op[A] = nop[A],
                                 sequence: => Op[A] = nop[A],
                                 attribute: => Op[A] = nop[A],
                                 attributeGroup: => Op[A] = nop[A],
                                 anyAttribute: => Op[A] = nop[A]): Op[A] =
    composeOps(foldAttributes(Eval.later(Seq("id" -> id, "base" -> base))))(
      foldChild(
        Eval.later(
          Seq(
            "annotation" -> annotation,
            "group" -> group,
            "all" -> all,
            "choice" -> choice,
            "sequence" -> sequence,
            "attribute" -> attribute,
            "attributeGroup" -> attributeGroup,
            "anyAttribute" -> anyAttribute
          )
        )
      )
    )

  def group[A](id: => AttrOp[A] = attrNop[A],
               name: => AttrOp[A] = attrNop[A],
               maxOccurs: => AttrOp[A] = attrNop[A],
               minOccurs: => AttrOp[A] = attrNop[A],
               ref: => AttrOp[A] = attrNop[A],
               annotation: => Op[A] = nop[A],
               all: => Op[A] = nop[A],
               choice: => Op[A] = nop[A],
               sequence: => Op[A] = nop[A]): Op[A] =
    composeOps(
      foldAttributes(
        Eval.later(
          Seq(
            "id" -> id,
            "name" -> name,
            "maxOccurs" -> maxOccurs,
            "minOccurs" -> minOccurs,
            "ref" -> ref
          )
        )
      )
    )(
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
    )

  def all[A](id: => AttrOp[A] = attrNop[A],
             maxOccurs: => AttrOp[A] = attrNop[A],
             minOccurs: => AttrOp[A] = attrNop[A],
             annotation: => Op[A] = nop[A],
             element: => Op[A] = nop[A]): Op[A] =
    composeOps(
      foldAttributes(
        Eval.later(
          Seq("id" -> id, "maxOccurs" -> maxOccurs, "minOccurs" -> minOccurs)
        )
      )
    )(
      foldChild(
        Eval.later(Seq("annotation" -> annotation, "element" -> element))
      )
    )

  def choice[A](id: => AttrOp[A] = attrNop[A],
                maxOccurs: => AttrOp[A] = attrNop[A],
                minOccurs: => AttrOp[A] = attrNop[A],
                annotation: => Op[A] = nop[A],
                element: => Op[A] = nop[A],
                group: => Op[A] = nop[A],
                choice: => Op[A] = nop[A],
                sequence: => Op[A] = nop[A],
                any: => Op[A] = nop[A]): Op[A] =
    composeOps(
      foldAttributes(
        Eval.later(
          Seq("id" -> id, "maxOccurs" -> maxOccurs, "minOccurs" -> minOccurs)
        )
      )
    )(
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
    )

  def sequence[A](id: => AttrOp[A] = attrNop[A],
                  maxOccurs: => AttrOp[A] = attrNop[A],
                  minOccurs: => AttrOp[A] = attrNop[A],
                  annotation: => Op[A] = nop[A],
                  element: => Op[A] = nop[A],
                  group: => Op[A] = nop[A],
                  choice: => Op[A] = nop[A],
                  sequence: => Op[A] = nop[A],
                  any: => Op[A] = nop[A]): Op[A] =
    composeOps(
      foldAttributes(
        Eval.later(
          Seq("id" -> id, "maxOccurs" -> maxOccurs, "minOccurs" -> minOccurs)
        )
      )
    )(
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
    )

  def any[A](id: => AttrOp[A] = attrNop[A],
             maxOccurs: => AttrOp[A] = attrNop[A],
             minOccurs: => AttrOp[A] = attrNop[A],
             namespace: => AttrOp[A] = attrNop[A],
             processContents: => AttrOp[A] = attrNop[A],
             annotation: => Op[A] = nop[A]): Op[A] =
    composeOps(
      foldAttributes(
        Eval.later(
          Seq(
            "id" -> id,
            "maxOccurs" -> maxOccurs,
            "minOccurs" -> minOccurs,
            "namespace" -> namespace,
            "processContents" -> processContents
          )
        )
      )
    )(foldChild(Eval.later(Seq("annotation" -> annotation))))

  def simpleContent[A](id: => AttrOp[A] = attrNop[A],
                       annotation: => Op[A] = nop[A],
                       restriction: => Op[A] = nop[A],
                       extension: => Op[A] = nop[A]): Op[A] =
    composeOps(foldAttributes(Eval.later(Seq("id" -> id))))(
      foldChild(
        Eval.later(
          Seq(
            "annotation" -> annotation,
            "restriction" -> restriction,
            "extension" -> extension
          )
        )
      )
    )

  def simpleContentRestriction[A](id: => AttrOp[A] = attrNop[A],
                                  base: => AttrOp[A] = attrNop[A],
                                  annotation: => Op[A] = nop[A],
                                  simpleType: => Op[A] = nop[A],
                                  minExclusive: => Op[A] = nop[A],
                                  minInclusive: => Op[A] = nop[A],
                                  maxExclusive: => Op[A] = nop[A],
                                  maxInclusive: => Op[A] = nop[A],
                                  totalDigits: => Op[A] = nop[A],
                                  fractionDigits: => Op[A] = nop[A],
                                  length: => Op[A] = nop[A],
                                  minLength: => Op[A] = nop[A],
                                  maxLength: => Op[A] = nop[A],
                                  enumeration: => Op[A] = nop[A],
                                  whiteSpace: => Op[A] = nop[A],
                                  pattern: => Op[A] = nop[A],
                                  attribute: => Op[A] = nop[A],
                                  attributeGroup: => Op[A] = nop[A],
                                  anyAttribute: => Op[A] = nop[A]): Op[A] =
    composeOps(foldAttributes(Eval.later(Seq("id" -> id, "base" -> base))))(
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
    )

  def simpleContentExtension[A](id: => AttrOp[A] = attrNop[A],
                                base: => AttrOp[A] = attrNop[A],
                                annotation: => Op[A] = nop[A],
                                attribute: => Op[A] = nop[A],
                                attributeGroup: => Op[A] = nop[A],
                                anyAttribute: => Op[A] = nop[A]): Op[A] =
    composeOps(foldAttributes(Eval.later(Seq("id" -> id, "base" -> base))))(
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
    )

  def attribute[A](id: => AttrOp[A] = attrNop[A],
                   default: => AttrOp[A] = attrNop[A],
                   fixed: => AttrOp[A] = attrNop[A],
                   form: => AttrOp[A] = attrNop[A],
                   name: => AttrOp[A] = attrNop[A],
                   ref: => AttrOp[A] = attrNop[A],
                   `type`: => AttrOp[A] = attrNop[A],
                   use: => AttrOp[A] = attrNop[A],
                   annotation: => Op[A] = nop[A],
                   simpleType: => Op[A] = nop[A]): Op[A] =
    composeOps(
      foldAttributes(
        Eval.later(
          Seq(
            "id" -> id,
            "default" -> default,
            "fixed" -> fixed,
            "form" -> form,
            "name" -> name,
            "ref" -> ref,
            "type" -> `type`,
            "use" -> use
          )
        )
      )
    )(
      foldChild(
        Eval.later(Seq("annotation" -> annotation, "simpleType" -> simpleType))
      )
    )

  def attributeGroup[A](id: => AttrOp[A] = attrNop[A],
                        name: => AttrOp[A] = attrNop[A],
                        ref: => AttrOp[A] = attrNop[A],
                        annotation: => Op[A] = nop[A],
                        attribute: => Op[A] = nop[A],
                        attributeGroup: => Op[A] = nop[A],
                        anyAttribute: => Op[A] = nop[A]): Op[A] =
    composeOps(
      foldAttributes(Eval.later(Seq("id" -> id, "name" -> name, "ref" -> ref)))
    )(
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
    )

  def anyAttribute[A](id: => AttrOp[A] = attrNop[A],
                      namespace: => AttrOp[A] = attrNop[A],
                      processContents: => AttrOp[A] = attrNop[A],
                      annotation: => Op[A] = nop[A]): Op[A] =
    composeOps(
      foldAttributes(
        Eval.later(
          Seq(
            "id" -> id,
            "namespace" -> namespace,
            "processContents" -> processContents
          )
        )
      )
    )(foldChild(Eval.later(Seq("annotation" -> annotation))))

  def element[A](id: => AttrOp[A] = attrNop[A],
                 `abstract`: => AttrOp[A] = attrNop[A],
                 block: => AttrOp[A] = attrNop[A],
                 default: => AttrOp[A] = attrNop[A],
                 `final`: => AttrOp[A] = attrNop[A],
                 fixed: => AttrOp[A] = attrNop[A],
                 form: => AttrOp[A] = attrNop[A],
                 maxOccurs: => AttrOp[A] = attrNop[A],
                 minOccurs: => AttrOp[A] = attrNop[A],
                 name: => AttrOp[A] = attrNop[A],
                 nillable: => AttrOp[A] = attrNop[A],
                 ref: => AttrOp[A] = attrNop[A],
                 substitutionGroup: => AttrOp[A] = attrNop[A],
                 `type`: => AttrOp[A] = attrNop[A],
                 annotation: => Op[A] = nop[A],
                 simpleType: => Op[A] = nop[A],
                 complexType: => Op[A] = nop[A],
                 unique: => Op[A] = nop[A],
                 key: => Op[A] = nop[A],
                 keyref: => Op[A] = nop[A]): Op[A] =
    composeOps(
      foldAttributes(
        Eval.later(
          Seq(
            "id" -> id,
            "abstract" -> `abstract`,
            "block" -> block,
            "default" -> default,
            "final" -> `final`,
            "fixed" -> fixed,
            "form" -> form,
            "maxOccurs" -> maxOccurs,
            "minOccurs" -> minOccurs,
            "name" -> name,
            "nillable" -> nillable,
            "ref" -> ref,
            "substitutionGroup" -> substitutionGroup,
            "type" -> `type`
          )
        )
      )
    )(
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
    )

  def unique[A](id: => AttrOp[A] = attrNop[A],
                name: => AttrOp[A] = attrNop[A],
                annotation: => Op[A] = nop[A],
                selector: => Op[A] = nop[A],
                field: => Op[A] = nop[A]): Op[A] =
    composeOps(foldAttributes(Eval.later(Seq("id" -> id, "name" -> name))))(
      foldChild(
        Eval.later(
          Seq(
            "annotation" -> annotation,
            "selector" -> selector,
            "field" -> field
          )
        )
      )
    )

  def key[A](id: => AttrOp[A] = attrNop[A],
             name: => AttrOp[A] = attrNop[A],
             annotation: => Op[A] = nop[A],
             selector: => Op[A] = nop[A],
             field: => Op[A] = nop[A]): Op[A] =
    composeOps(foldAttributes(Eval.later(Seq("id" -> id, "name" -> name))))(
      foldChild(
        Eval.later(
          Seq(
            "annotation" -> annotation,
            "selector" -> selector,
            "field" -> field
          )
        )
      )
    )

  def keyref[A](id: => AttrOp[A] = attrNop[A],
                name: => AttrOp[A] = attrNop[A],
                referer: => AttrOp[A] = attrNop[A],
                annotation: => Op[A] = nop[A],
                selector: => Op[A] = nop[A],
                field: => Op[A] = nop[A]): Op[A] =
    composeOps(
      foldAttributes(
        Eval.later(Seq("id" -> id, "name" -> name, "referer" -> referer))
      )
    )(
      foldChild(
        Eval.later(
          Seq(
            "annotation" -> annotation,
            "selector" -> selector,
            "field" -> field
          )
        )
      )
    )

  def selector[A](id: => AttrOp[A] = attrNop[A],
                  xpath: => AttrOp[A] = attrNop[A],
                  annotation: => Op[A] = nop[A]): Op[A] =
    composeOps(foldAttributes(Eval.later(Seq("id" -> id, "xpath" -> xpath))))(
      foldChild(Eval.later(Seq("annotation" -> annotation)))
    )

  def field[A](id: => AttrOp[A] = attrNop[A],
               xpath: => AttrOp[A] = attrNop[A],
               annotation: => Op[A] = nop[A]): Op[A] =
    composeOps(foldAttributes(Eval.later(Seq("id" -> id, "xpath" -> xpath))))(
      foldChild(Eval.later(Seq("annotation" -> annotation)))
    )

  def nop[A]: Op[A] = a => _ => ok(a)

  def attrNop[A]: AttrOp[A] = a => _ => ok(a)
}
