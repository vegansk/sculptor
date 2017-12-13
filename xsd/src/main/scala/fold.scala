package sculptor.xsd

import scala.xml._
import cats._, cats.data._, cats.implicits._
import shapeless.tag, tag.@@

object fold {

  sealed trait FormDefault
  case object Qualified extends FormDefault
  case object Unqualified extends FormDefault

  implicit val formDefaultEqInstance: Eq[FormDefault] = new Eq[FormDefault] {
    @SuppressWarnings(Array("org.wartremover.warts.Equals"))
    def eqv(a: FormDefault, b: FormDefault) = a == b
  }

  final case class FoldState( // xsd namespace prefix
                             schemaNs: Option[String] = None,
                             attributeFormDefault: FormDefault = Unqualified,
                             elementFormDefault: FormDefault = Unqualified,
                             // The path to the current processing element
                             path: xml.Path = xml.mkPath,
                             // Append path to the error message
                             appendPathToError: Boolean = false,
                             errorRaised: Boolean = false,
                             // In strict mode, all the childs and attributes must be processed
                             strictMode: Boolean = false)

  type FS[A] = State[FoldState, A]

  type Result[A] = EitherT[FS, String, A]

  type Op[A] = Node => A => Result[A]

  type AttrOp[A] = String => A => Result[A]

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

  def getAppendPathToError: Result[Boolean] =
    liftS(State.get[FoldState].map(_.appendPathToError))

  def getErrorRaised: Result[Boolean] =
    liftS(State.get[FoldState].map(_.errorRaised))

  def setErrorRaised(errorRaised: Boolean): Result[Unit] =
    liftS(State.modify[FoldState](_.copy(errorRaised = errorRaised)))

  def getStrictMode: Result[Boolean] =
    liftS(State.get[FoldState].map(_.strictMode))

  def getPath: Result[xml.Path] =
    liftS(State.get[FoldState].map(_.path))

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

  private[xsd] def findSchemaNs(xsd: Node): Result[Option[String]] =
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

  private[xsd] val findAttributeFormDefault = findFormDefault(
    "attributeFormDefault"
  ) _

  private[xsd] val findElementFormDefault = findFormDefault(
    "elementFormDefault"
  ) _

  private def filterNs(schemaNs: Option[String],
                       ns: Option[String],
                       default: FormDefault): Boolean =
    ns match {
      case None if default === Qualified => false
      case None if default === Unqualified => true
      case ns if ns === schemaNs => true
      case _ => false
    }

  private def xsdLabel(n: Node): Result[String] =
    for {
      ns <- getSchemaNs
      elementFormDefault <- getElementFormDefault
      _ <- if (!filterNs(ns, Option(n.prefix), elementFormDefault))
        error(
          s"Element's prefix `${xml.fullName(n)}` doesn't match the xml schema prefix `${ns.getOrElse("")}`"
        )
      else ok(())
    } yield n.label

  private[xsd] def xsdChild(n: Node): Result[List[Node]] =
    for {
      ns <- getSchemaNs
      elementFormDefault <- getElementFormDefault
      res <- ok(
        n.child.toList
          .filter(_ match {
            case _: Elem => true
            case _ => false
          })
          .filter(ch => filterNs(ns, Option(ch.prefix), elementFormDefault))
      )
    } yield res

  def composeOps[A](x: Op[A])(y: Op[A]): Op[A] =
    n =>
      a =>
        for {
          a1 <- x(n)(a)
          a2 <- y(n)(a1)
        } yield a2

  def handleError[A](res: Result[A]): Result[A] =
    res.handleErrorWith { err0 =>
      for {
        appendPathToError <- getAppendPathToError
        errorRaised <- getErrorRaised
        _ <- setErrorRaised(true)
        path <- getPath
        a <- {
          err0 + (
            if (appendPathToError && !errorRaised)
              s" [${xml.pathToString(path)}]"
            else ""
          )
        }.raiseError[Result, A]
      } yield a
    }

  private[xsd] def foldChild[A](handlers: Handlers[Op, A]): Op[A] =
    n0 =>
      a0 =>
        withNode(n0) {
          val res = for {
            h <- ok(Map(handlers.value: _*))
            child <- xsdChild(n0)
            res <- child.foldLeftM(a0) { (a, n) =>
              h.get(n.label)
                .fold(
                  error[A](s"Unknown element of type `${xml.fullName(n)}`")
                )(_(n)(a))
            }
          } yield res
          handleError(res)
    }

  private[xsd] def xsdAttributes(n: Node): Result[List[Attribute]] =
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
    n =>
      a =>
        withNode(n) {
          val res = for {
            h <- ok(Map(handlers.value: _*))
            attrs <- xsdAttributes(n)
            res <- attrs.foldLeftM(a) { (a, attr) =>
              h.get(attr.key)
                .fold(error[A](s"Unknown attribute `${attr.prefixedKey}`"))(
                  _(attr.value.text)(a)
                )
            }
          } yield res
          handleError(res)
    }

  trait SchemaTag
  trait IdTag
  trait TargetNamespaceTag
  trait AnnotationTag
  trait SimpleTypeTag
  trait ComplexTypeTag
  trait ElementTag
  trait AttributeTag
  trait AppinfoTag
  trait DocumentationTag
  trait FinalTag
  trait NameTag
  trait ListTag
  trait SimpleTypeRestrictionTag
  trait UnionTag
  trait ItemTypeTag
  trait BaseTag
  trait MinExclusiveTag
  trait MinInclusiveTag
  trait MaxExclusiveTag
  trait MaxInclusiveTag
  trait TotalDigitsTag
  trait FractionDigitsTag
  trait LengthTag
  trait MinLengthTag
  trait MaxLengthTag
  trait EnumerationTag
  trait WhiteSpaceTag
  trait PatternTag
  trait MemberTypesTag
  trait AbstractTag
  trait BlockTag
  trait MixedTag
  trait SimpleContentTag
  trait ComplexContentTag
  trait GroupTag
  trait AllTag
  trait ChoiceTag
  trait SequenceTag
  trait ComplexContentRestrictionTag
  trait ComplexContentExtensionTag
  trait AttributeGroupTag
  trait AnyAttributeTag
  trait MaxOccursTag
  trait MinOccursTag
  trait RefTag
  trait AnyTag
  trait NamespaceTag
  trait ProcessContentsTag
  trait SimpleContentRestrictionTag
  trait SimpleContentExtensionTag
  trait DefaultTag
  trait FixedTag
  trait FormTag
  trait TypeTag
  trait UseTag
  trait NillableTag
  trait SubstitutionGroupTag
  trait UniqueTag
  trait KeyTag
  trait KeyrefTag
  trait SelectorTag
  trait FieldTag
  trait RefererTag
  trait XpathTag
  trait ValueTag

  type IdOp[A] = AttrOp[A] @@ IdTag
  def idOp[A](op: AttrOp[A]): IdOp[A] = tag[IdTag][AttrOp[A]](op)
  type TargetNamespaceOp[A] = AttrOp[A] @@ TargetNamespaceTag
  def targetNamespaceOp[A](op: AttrOp[A]): TargetNamespaceOp[A] =
    tag[TargetNamespaceTag][AttrOp[A]](op)
  type FinalOp[A] = AttrOp[A] @@ FinalTag
  def finalOp[A](op: AttrOp[A]): FinalOp[A] = tag[FinalTag][AttrOp[A]](op)
  type NameOp[A] = AttrOp[A] @@ NameTag
  def nameOp[A](op: AttrOp[A]): NameOp[A] = tag[NameTag][AttrOp[A]](op)
  type ItemTypeOp[A] = AttrOp[A] @@ ItemTypeTag
  def itemTypeOp[A](op: AttrOp[A]): ItemTypeOp[A] =
    tag[ItemTypeTag][AttrOp[A]](op)
  type BaseOp[A] = AttrOp[A] @@ BaseTag
  def baseOp[A](op: AttrOp[A]): BaseOp[A] = tag[BaseTag][AttrOp[A]](op)
  type MemberTypesOp[A] = AttrOp[A] @@ MemberTypesTag
  def memberTypesOp[A](op: AttrOp[A]): MemberTypesOp[A] =
    tag[MemberTypesTag][AttrOp[A]](op)
  type AbstractOp[A] = AttrOp[A] @@ AbstractTag
  def abstractOp[A](op: AttrOp[A]): AbstractOp[A] =
    tag[AbstractTag][AttrOp[A]](op)
  type BlockOp[A] = AttrOp[A] @@ BlockTag
  def blockOp[A](op: AttrOp[A]): BlockOp[A] = tag[BlockTag][AttrOp[A]](op)
  type MixedOp[A] = AttrOp[A] @@ MixedTag
  def mixedOp[A](op: AttrOp[A]): MixedOp[A] = tag[MixedTag][AttrOp[A]](op)
  type MaxOccursOp[A] = AttrOp[A] @@ MaxOccursTag
  def maxOccursOp[A](op: AttrOp[A]): MaxOccursOp[A] =
    tag[MaxOccursTag][AttrOp[A]](op)
  type MinOccursOp[A] = AttrOp[A] @@ MinOccursTag
  def minOccursOp[A](op: AttrOp[A]): MinOccursOp[A] =
    tag[MinOccursTag][AttrOp[A]](op)
  type RefOp[A] = AttrOp[A] @@ RefTag
  def refOp[A](op: AttrOp[A]): RefOp[A] = tag[RefTag][AttrOp[A]](op)
  type NamespaceOp[A] = AttrOp[A] @@ NamespaceTag
  def namespaceOp[A](op: AttrOp[A]): NamespaceOp[A] =
    tag[NamespaceTag][AttrOp[A]](op)
  type ProcessContentsOp[A] = AttrOp[A] @@ ProcessContentsTag
  def processContentsOp[A](op: AttrOp[A]): ProcessContentsOp[A] =
    tag[ProcessContentsTag][AttrOp[A]](op)
  type DefaultOp[A] = AttrOp[A] @@ DefaultTag
  def defaultOp[A](op: AttrOp[A]): DefaultOp[A] =
    tag[DefaultTag][AttrOp[A]](op)
  type FixedOp[A] = AttrOp[A] @@ FixedTag
  def fixedOp[A](op: AttrOp[A]): FixedOp[A] = tag[FixedTag][AttrOp[A]](op)
  type FormOp[A] = AttrOp[A] @@ FormTag
  def formOp[A](op: AttrOp[A]): FormOp[A] = tag[FormTag][AttrOp[A]](op)
  type TypeOp[A] = AttrOp[A] @@ TypeTag
  def typeOp[A](op: AttrOp[A]): TypeOp[A] = tag[TypeTag][AttrOp[A]](op)
  type UseOp[A] = AttrOp[A] @@ UseTag
  def useOp[A](op: AttrOp[A]): UseOp[A] = tag[UseTag][AttrOp[A]](op)
  type NillableOp[A] = AttrOp[A] @@ NillableTag
  def nillableOp[A](op: AttrOp[A]): NillableOp[A] =
    tag[NillableTag][AttrOp[A]](op)
  type SubstitutionGroupOp[A] = AttrOp[A] @@ SubstitutionGroupTag
  def substitutionGroupOp[A](op: AttrOp[A]): SubstitutionGroupOp[A] =
    tag[SubstitutionGroupTag][AttrOp[A]](op)
  type RefererOp[A] = AttrOp[A] @@ RefererTag
  def refererOp[A](op: AttrOp[A]): RefererOp[A] =
    tag[RefererTag][AttrOp[A]](op)
  type XpathOp[A] = AttrOp[A] @@ XpathTag
  def xpathOp[A](op: AttrOp[A]): XpathOp[A] = tag[XpathTag][AttrOp[A]](op)
  type ValueOp[A] = AttrOp[A] @@ ValueTag
  def valueOp[A](op: AttrOp[A]): ValueOp[A] = tag[ValueTag][AttrOp[A]](op)

  type SchemaOp[A] = Op[A] @@ SchemaTag
  type AnnotationOp[A] = Op[A] @@ AnnotationTag
  type SimpleTypeOp[A] = Op[A] @@ SimpleTypeTag
  type ComplexTypeOp[A] = Op[A] @@ ComplexTypeTag
  type ElementOp[A] = Op[A] @@ ElementTag
  type AttributeOp[A] = Op[A] @@ AttributeTag
  type AppinfoOp[A] = Op[A] @@ AppinfoTag
  def appinfoOp[A](op: Op[A]): AppinfoOp[A] =
    tag[AppinfoTag][Op[A]](op)
  type DocumentationOp[A] = Op[A] @@ DocumentationTag
  def documentationOp[A](op: Op[A]): DocumentationOp[A] =
    tag[DocumentationTag][Op[A]](op)
  type ListOp[A] = Op[A] @@ ListTag
  type SimpleTypeRestrictionOp[A] = Op[A] @@ SimpleTypeRestrictionTag
  type UnionOp[A] = Op[A] @@ UnionTag
  type MinExclusiveOp[A] = Op[A] @@ MinExclusiveTag
  type MinInclusiveOp[A] = Op[A] @@ MinInclusiveTag
  type MaxExclusiveOp[A] = Op[A] @@ MaxExclusiveTag
  type MaxInclusiveOp[A] = Op[A] @@ MaxInclusiveTag
  type TotalDigitsOp[A] = Op[A] @@ TotalDigitsTag
  type FractionDigitsOp[A] = Op[A] @@ FractionDigitsTag
  type LengthOp[A] = Op[A] @@ LengthTag
  type MinLengthOp[A] = Op[A] @@ MinLengthTag
  type MaxLengthOp[A] = Op[A] @@ MaxLengthTag
  type EnumerationOp[A] = Op[A] @@ EnumerationTag
  type WhiteSpaceOp[A] = Op[A] @@ WhiteSpaceTag
  type PatternOp[A] = Op[A] @@ PatternTag
  type SimpleContentOp[A] = Op[A] @@ SimpleContentTag
  type ComplexContentOp[A] = Op[A] @@ ComplexContentTag
  type GroupOp[A] = Op[A] @@ GroupTag
  type AllOp[A] = Op[A] @@ AllTag
  type ChoiceOp[A] = Op[A] @@ ChoiceTag
  type SequenceOp[A] = Op[A] @@ SequenceTag
  type ComplexContentRestrictionOp[A] = Op[A] @@ ComplexContentRestrictionTag
  type ComplexContentExtensionOp[A] = Op[A] @@ ComplexContentExtensionTag
  type AttributeGroupOp[A] = Op[A] @@ AttributeGroupTag
  type AnyAttributeOp[A] = Op[A] @@ AnyAttributeTag
  type AnyOp[A] = Op[A] @@ AnyTag
  type SimpleContentRestrictionOp[A] = Op[A] @@ SimpleContentRestrictionTag
  type SimpleContentExtensionOp[A] = Op[A] @@ SimpleContentExtensionTag
  type UniqueOp[A] = Op[A] @@ UniqueTag
  type KeyOp[A] = Op[A] @@ KeyTag
  type KeyrefOp[A] = Op[A] @@ KeyrefTag
  type SelectorOp[A] = Op[A] @@ SelectorTag
  type FieldOp[A] = Op[A] @@ FieldTag

  private def schema0[A](id: => IdOp[A],
                         targetNamespace: => TargetNamespaceOp[A],
                         annotation: => AnnotationOp[A],
                         simpleType: => SimpleTypeOp[A],
                         complexType: => ComplexTypeOp[A],
                         element: => ElementOp[A],
                         attribute: => AttributeOp[A]): SchemaOp[A] =
    tag[SchemaTag][Op[A]](
      composeOps(
        foldAttributes(
          Eval.later(
            Seq(
              "id" -> id,
              "targetNamespace" -> targetNamespace,
              "elementFormDefault" -> attrNop[A],
              "attributeFormDefault" -> attrNop[A]
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
              "element" -> element,
              "attribute" -> attribute
            )
          )
        )
      )
    )

  def schema[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    targetNamespace: => TargetNamespaceOp[A] =
      tag[TargetNamespaceTag](attrNop0[A]("targetNamespace")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    simpleType: => SimpleTypeOp[A] = tag[SimpleTypeTag](nop0[A]("simpleType")),
    complexType: => ComplexTypeOp[A] =
      tag[ComplexTypeTag](nop0[A]("complexType")),
    element: => ElementOp[A] = tag[ElementTag](nop0[A]("element")),
    attribute: => AttributeOp[A] = tag[AttributeTag](nop0[A]("attribute"))
  ): SchemaOp[A] =
    tag[SchemaTag][Op[A]](
      n =>
        a =>
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
                )(n)(a)
              case l => error[A](s"`schema` tag was expected, found `$l`")
            }
          } yield res
    )

  def annotationOp[A](op: Op[A]): AnnotationOp[A] =
    tag[AnnotationTag][Op[A]](op)

  def annotation[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    appinfo: => AppinfoOp[A] = tag[AppinfoTag](nop0[A]("appinfo")),
    documentation: => DocumentationOp[A] =
      tag[DocumentationTag](nop0[A]("documentation"))
  ): AnnotationOp[A] =
    annotationOp(
      composeOps(foldAttributes(Eval.later(Seq("id" -> id))))(
        foldChild(
          Eval
            .later(Seq("appinfo" -> appinfo, "documentation" -> documentation))
        )
      )
    )

  def simpleTypeOp[A](op: Op[A]): SimpleTypeOp[A] =
    tag[SimpleTypeTag][Op[A]](op)

  def simpleType[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    `final`: => FinalOp[A] = tag[FinalTag](attrNop0[A]("final")),
    name: => NameOp[A] = tag[NameTag](attrNop0[A]("name")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    list: => ListOp[A] = tag[ListTag](nop0[A]("list")),
    restriction: => SimpleTypeRestrictionOp[A] =
      tag[SimpleTypeRestrictionTag](nop0[A]("restriction")),
    union: => UnionOp[A] = tag[UnionTag](nop0[A]("union"))
  ): SimpleTypeOp[A] =
    simpleTypeOp(
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
    )

  def listOp[A](op: Op[A]): ListOp[A] = tag[ListTag][Op[A]](op)

  def list[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    itemType: => ItemTypeOp[A] = tag[ItemTypeTag](attrNop0[A]("itemType")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    simpleType: => SimpleTypeOp[A] = tag[SimpleTypeTag](nop0[A]("simpleType"))
  ): ListOp[A] =
    listOp(
      composeOps(
        foldAttributes(Eval.later(Seq("id" -> id, "itemType" -> itemType)))
      )(
        foldChild(
          Eval
            .later(Seq("annotation" -> annotation, "simpleType" -> simpleType))
        )
      )
    )

  def simpleTypeRestrictionOp[A](op: Op[A]): SimpleTypeRestrictionOp[A] =
    tag[SimpleTypeRestrictionTag][Op[A]](op)

  def simpleTypeRestriction[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    base: => BaseOp[A] = tag[BaseTag](attrNop0[A]("base")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    simpleType: => SimpleTypeOp[A] = tag[SimpleTypeTag](nop0[A]("simpleType")),
    minExclusive: => MinExclusiveOp[A] =
      tag[MinExclusiveTag](nop0[A]("minExclusive")),
    minInclusive: => MinInclusiveOp[A] =
      tag[MinInclusiveTag](nop0[A]("minInclusive")),
    maxExclusive: => MaxExclusiveOp[A] =
      tag[MaxExclusiveTag](nop0[A]("maxExclusive")),
    maxInclusive: => MaxInclusiveOp[A] =
      tag[MaxInclusiveTag](nop0[A]("maxInclusive")),
    totalDigits: => TotalDigitsOp[A] =
      tag[TotalDigitsTag](nop0[A]("totalDigits")),
    fractionDigits: => FractionDigitsOp[A] =
      tag[FractionDigitsTag](nop0[A]("fractionDigits")),
    length: => LengthOp[A] = tag[LengthTag](nop0[A]("length")),
    minLength: => MinLengthOp[A] = tag[MinLengthTag](nop0[A]("minLength")),
    maxLength: => MaxLengthOp[A] = tag[MaxLengthTag](nop0[A]("maxLength")),
    enumeration: => EnumerationOp[A] =
      tag[EnumerationTag](nop0[A]("enumeration")),
    whiteSpace: => WhiteSpaceOp[A] = tag[WhiteSpaceTag](nop0[A]("whiteSpace")),
    pattern: => PatternOp[A] = tag[PatternTag](nop0[A]("pattern"))
  ): SimpleTypeRestrictionOp[A] =
    simpleTypeRestrictionOp(
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
    )

  def tagValue[A](
    value: => ValueOp[A] = tag[ValueTag](attrNop0[A]("value"))
  ): Op[A] =
    foldAttributes(Eval.later(Seq("value" -> value)))

  def whiteSpaceOp[A](op: Op[A]): WhiteSpaceOp[A] =
    tag[WhiteSpaceTag][Op[A]](op)

  def whiteSpace[A](
    value: => ValueOp[A] = tag[ValueTag](attrNop0[A]("value"))
  ): WhiteSpaceOp[A] = whiteSpaceOp(tagValue(value = value))

  def minLengthOp[A](op: Op[A]): MinLengthOp[A] = tag[MinLengthTag][Op[A]](op)

  def minLength[A](
    value: => ValueOp[A] = tag[ValueTag](attrNop0[A]("value"))
  ): MinLengthOp[A] = minLengthOp(tagValue(value = value))

  def maxLengthOp[A](op: Op[A]): MaxLengthOp[A] = tag[MaxLengthTag][Op[A]](op)

  def maxLength[A](
    value: => ValueOp[A] = tag[ValueTag](attrNop0[A]("value"))
  ): MaxLengthOp[A] = maxLengthOp(tagValue(value = value))

  def patternOp[A](op: Op[A]): PatternOp[A] = tag[PatternTag][Op[A]](op)

  def pattern[A](
    value: => ValueOp[A] = tag[ValueTag](attrNop0[A]("value"))
  ): PatternOp[A] = patternOp(tagValue(value = value))

  def enumerationOp[A](op: Op[A]): EnumerationOp[A] =
    tag[EnumerationTag][Op[A]](op)

  def enumeration[A](
    value: => ValueOp[A] = tag[ValueTag](attrNop0[A]("value")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation"))
  ): EnumerationOp[A] = enumerationOp(
    composeOps(foldAttributes(Eval.later(Seq("value" -> value))))(
      foldChild(Eval.later(Seq("annotation" -> annotation)))
    )
  )

  def unionOp[A](op: Op[A]): UnionOp[A] = tag[UnionTag][Op[A]](op)

  def union[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    memberTypes: => MemberTypesOp[A] =
      tag[MemberTypesTag](attrNop0[A]("memberTypes")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    simpleType: => SimpleTypeOp[A] = tag[SimpleTypeTag](nop0[A]("simpleType"))
  ): UnionOp[A] =
    unionOp(
      composeOps(
        foldAttributes(
          Eval.later(Seq("id" -> id, "memberTypes" -> memberTypes))
        )
      )(
        foldChild(
          Eval
            .later(Seq("annotation" -> annotation, "simpleType" -> simpleType))
        )
      )
    )

  def complexTypeOp[A](op: Op[A]): ComplexTypeOp[A] =
    tag[ComplexTypeTag][Op[A]](op)

  def complexType[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    `abstract`: => AbstractOp[A] = tag[AbstractTag](attrNop0[A]("abstract")),
    block: => BlockOp[A] = tag[BlockTag](attrNop0[A]("block")),
    `final`: => FinalOp[A] = tag[FinalTag](attrNop0[A]("final")),
    mixed: => MixedOp[A] = tag[MixedTag](attrNop0[A]("mixed")),
    name: => NameOp[A] = tag[NameTag](attrNop0[A]("name")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    simpleContent: => SimpleContentOp[A] =
      tag[SimpleContentTag](nop0[A]("simpleContent")),
    complexContent: => ComplexContentOp[A] =
      tag[ComplexContentTag](nop0[A]("complexContent")),
    group: => GroupOp[A] = tag[GroupTag](nop0[A]("group")),
    all: => AllOp[A] = tag[AllTag](nop0[A]("all")),
    choice: => ChoiceOp[A] = tag[ChoiceTag](nop0[A]("choice")),
    sequence: => SequenceOp[A] = tag[SequenceTag](nop0[A]("sequence")),
    attribute: => AttributeOp[A] = tag[AttributeTag](nop0[A]("attribute"))
  ): ComplexTypeOp[A] =
    complexTypeOp(
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
    )

  def complexContentOp[A](op: Op[A]): ComplexContentOp[A] =
    tag[ComplexContentTag][Op[A]](op)

  def complexContent[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    mixed: => MixedOp[A] = tag[MixedTag](attrNop0[A]("mixed")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    restriction: => ComplexContentRestrictionOp[A] =
      tag[ComplexContentRestrictionTag](nop0[A]("restriction")),
    extension: => ComplexContentExtensionOp[A] =
      tag[ComplexContentExtensionTag](nop0[A]("extension"))
  ): ComplexContentOp[A] =
    complexContentOp(
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
    )

  def complexContentRestrictionOp[A](
    op: Op[A]
  ): ComplexContentRestrictionOp[A] =
    tag[ComplexContentRestrictionTag][Op[A]](op)

  def complexContentRestriction[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    base: => BaseOp[A] = tag[BaseTag](attrNop0[A]("base")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    group: => GroupOp[A] = tag[GroupTag](nop0[A]("group")),
    all: => AllOp[A] = tag[AllTag](nop0[A]("all")),
    choice: => ChoiceOp[A] = tag[ChoiceTag](nop0[A]("choice")),
    sequence: => SequenceOp[A] = tag[SequenceTag](nop0[A]("sequence")),
    attribute: => AttributeOp[A] = tag[AttributeTag](nop0[A]("attribute")),
    attributeGroup: => AttributeGroupOp[A] =
      tag[AttributeGroupTag](nop0[A]("attributeGroup")),
    anyAttribute: => AnyAttributeOp[A] =
      tag[AnyAttributeTag](nop0[A]("anyAttribute"))
  ): ComplexContentRestrictionOp[A] =
    complexContentRestrictionOp(
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
    )

  def complexContentExtensionOp[A](op: Op[A]): ComplexContentExtensionOp[A] =
    tag[ComplexContentExtensionTag][Op[A]](op)

  def complexContentExtension[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    base: => BaseOp[A] = tag[BaseTag](attrNop0[A]("base")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    group: => GroupOp[A] = tag[GroupTag](nop0[A]("group")),
    all: => AllOp[A] = tag[AllTag](nop0[A]("all")),
    choice: => ChoiceOp[A] = tag[ChoiceTag](nop0[A]("choice")),
    sequence: => SequenceOp[A] = tag[SequenceTag](nop0[A]("sequence")),
    attribute: => AttributeOp[A] = tag[AttributeTag](nop0[A]("attribute")),
    attributeGroup: => AttributeGroupOp[A] =
      tag[AttributeGroupTag](nop0[A]("attributeGroup")),
    anyAttribute: => AnyAttributeOp[A] =
      tag[AnyAttributeTag](nop0[A]("anyAttribute"))
  ): ComplexContentExtensionOp[A] =
    complexContentExtensionOp(
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
    )

  def groupOp[A](op: Op[A]): GroupOp[A] = tag[GroupTag][Op[A]](op)

  def group[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    name: => NameOp[A] = tag[NameTag](attrNop0[A]("name")),
    maxOccurs: => MaxOccursOp[A] = tag[MaxOccursTag](attrNop0[A]("maxOccurs")),
    minOccurs: => MinOccursOp[A] = tag[MinOccursTag](attrNop0[A]("minOccurs")),
    ref: => RefOp[A] = tag[RefTag](attrNop0[A]("ref")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    all: => AllOp[A] = tag[AllTag](nop0[A]("all")),
    choice: => ChoiceOp[A] = tag[ChoiceTag](nop0[A]("choice")),
    sequence: => SequenceOp[A] = tag[SequenceTag](nop0[A]("sequence"))
  ): GroupOp[A] =
    groupOp(
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
    )

  def allOp[A](op: Op[A]): AllOp[A] = tag[AllTag][Op[A]](op)

  def all[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    maxOccurs: => MaxOccursOp[A] = tag[MaxOccursTag](attrNop0[A]("maxOccurs")),
    minOccurs: => MinOccursOp[A] = tag[MinOccursTag](attrNop0[A]("minOccurs")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    element: => ElementOp[A] = tag[ElementTag](nop0[A]("element"))
  ): AllOp[A] =
    allOp(
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
    )

  def choiceOp[A](op: Op[A]): ChoiceOp[A] = tag[ChoiceTag][Op[A]](op)

  def choice[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    maxOccurs: => MaxOccursOp[A] = tag[MaxOccursTag](attrNop0[A]("maxOccurs")),
    minOccurs: => MinOccursOp[A] = tag[MinOccursTag](attrNop0[A]("minOccurs")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    element: => ElementOp[A] = tag[ElementTag](nop0[A]("element")),
    group: => GroupOp[A] = tag[GroupTag](nop0[A]("group")),
    choice: => ChoiceOp[A] = tag[ChoiceTag](nop0[A]("choice")),
    sequence: => SequenceOp[A] = tag[SequenceTag](nop0[A]("sequence")),
    any: => AnyOp[A] = tag[AnyTag](nop0[A]("any"))
  ): ChoiceOp[A] =
    choiceOp(
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
    )

  def sequenceOp[A](op: Op[A]): SequenceOp[A] = tag[SequenceTag][Op[A]](op)

  def sequence[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    maxOccurs: => MaxOccursOp[A] = tag[MaxOccursTag](attrNop0[A]("maxOccurs")),
    minOccurs: => MinOccursOp[A] = tag[MinOccursTag](attrNop0[A]("minOccurs")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    element: => ElementOp[A] = tag[ElementTag](nop0[A]("element")),
    group: => GroupOp[A] = tag[GroupTag](nop0[A]("group")),
    choice: => ChoiceOp[A] = tag[ChoiceTag](nop0[A]("choice")),
    sequence: => SequenceOp[A] = tag[SequenceTag](nop0[A]("sequence")),
    any: => AnyOp[A] = tag[AnyTag](nop0[A]("any"))
  ): SequenceOp[A] =
    sequenceOp(
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
    )

  def anyOp[A](op: Op[A]): AnyOp[A] = tag[AnyTag][Op[A]](op)

  def any[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    maxOccurs: => MaxOccursOp[A] = tag[MaxOccursTag](attrNop0[A]("maxOccurs")),
    minOccurs: => MinOccursOp[A] = tag[MinOccursTag](attrNop0[A]("minOccurs")),
    namespace: => NamespaceOp[A] = tag[NamespaceTag](attrNop0[A]("namespace")),
    processContents: => ProcessContentsOp[A] =
      tag[ProcessContentsTag](attrNop0[A]("processContents")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation"))
  ): AnyOp[A] =
    anyOp(
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
    )

  def simpleContentOp[A](op: Op[A]): SimpleContentOp[A] =
    tag[SimpleContentTag][Op[A]](op)

  def simpleContent[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    restriction: => SimpleContentRestrictionOp[A] =
      tag[SimpleContentRestrictionTag](nop0[A]("restriction")),
    extension: => SimpleContentExtensionOp[A] =
      tag[SimpleContentExtensionTag](nop0[A]("extension"))
  ): SimpleContentOp[A] =
    simpleContentOp(
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
    )

  def simpleContentRestrictionOp[A](op: Op[A]): SimpleContentRestrictionOp[A] =
    tag[SimpleContentRestrictionTag][Op[A]](op)

  def simpleContentRestriction[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    base: => BaseOp[A] = tag[BaseTag](attrNop0[A]("base")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    simpleType: => SimpleTypeOp[A] = tag[SimpleTypeTag](nop0[A]("simpleType")),
    minExclusive: => MinExclusiveOp[A] =
      tag[MinExclusiveTag](nop0[A]("minExclusive")),
    minInclusive: => MinInclusiveOp[A] =
      tag[MinInclusiveTag](nop0[A]("minInclusive")),
    maxExclusive: => MaxExclusiveOp[A] =
      tag[MaxExclusiveTag](nop0[A]("maxExclusive")),
    maxInclusive: => MaxInclusiveOp[A] =
      tag[MaxInclusiveTag](nop0[A]("maxInclusive")),
    totalDigits: => TotalDigitsOp[A] =
      tag[TotalDigitsTag](nop0[A]("totalDigits")),
    fractionDigits: => FractionDigitsOp[A] =
      tag[FractionDigitsTag](nop0[A]("fractionDigits")),
    length: => LengthOp[A] = tag[LengthTag](nop0[A]("length")),
    minLength: => MinLengthOp[A] = tag[MinLengthTag](nop0[A]("minLength")),
    maxLength: => MaxLengthOp[A] = tag[MaxLengthTag](nop0[A]("maxLength")),
    enumeration: => EnumerationOp[A] =
      tag[EnumerationTag](nop0[A]("enumeration")),
    whiteSpace: => WhiteSpaceOp[A] = tag[WhiteSpaceTag](nop0[A]("whiteSpace")),
    pattern: => PatternOp[A] = tag[PatternTag](nop0[A]("pattern")),
    attribute: => AttributeOp[A] = tag[AttributeTag](nop0[A]("attribute")),
    attributeGroup: => AttributeGroupOp[A] =
      tag[AttributeGroupTag](nop0[A]("attributeGroup")),
    anyAttribute: => AnyAttributeOp[A] =
      tag[AnyAttributeTag](nop0[A]("anyAttribute"))
  ): SimpleContentRestrictionOp[A] =
    simpleContentRestrictionOp(
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
    )

  def simpleContentExtensionOp[A](op: Op[A]): SimpleContentExtensionOp[A] =
    tag[SimpleContentExtensionTag][Op[A]](op)

  def simpleContentExtension[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    base: => BaseOp[A] = tag[BaseTag](attrNop0[A]("base")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    attribute: => AttributeOp[A] = tag[AttributeTag](nop0[A]("attribute")),
    attributeGroup: => AttributeGroupOp[A] =
      tag[AttributeGroupTag](nop0[A]("attributeGroup")),
    anyAttribute: => AnyAttributeOp[A] =
      tag[AnyAttributeTag](nop0[A]("anyAttribute"))
  ): SimpleContentExtensionOp[A] =
    simpleContentExtensionOp(
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
    )

  def attributeOp[A](op: Op[A]): AttributeOp[A] = tag[AttributeTag][Op[A]](op)

  def attribute[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    default: => DefaultOp[A] = tag[DefaultTag](attrNop0[A]("default")),
    fixed: => FixedOp[A] = tag[FixedTag](attrNop0[A]("fixed")),
    form: => FormOp[A] = tag[FormTag](attrNop0[A]("form")),
    name: => NameOp[A] = tag[NameTag](attrNop0[A]("name")),
    ref: => RefOp[A] = tag[RefTag](attrNop0[A]("ref")),
    `type`: => TypeOp[A] = tag[TypeTag](attrNop0[A]("type")),
    use: => UseOp[A] = tag[UseTag](attrNop0[A]("use")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    simpleType: => SimpleTypeOp[A] = tag[SimpleTypeTag](nop0[A]("simpleType"))
  ): AttributeOp[A] =
    attributeOp(
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
          Eval
            .later(Seq("annotation" -> annotation, "simpleType" -> simpleType))
        )
      )
    )

  def attributeGroupOp[A](op: Op[A]): AttributeGroupOp[A] =
    tag[AttributeGroupTag][Op[A]](op)

  def attributeGroup[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    name: => NameOp[A] = tag[NameTag](attrNop0[A]("name")),
    ref: => RefOp[A] = tag[RefTag](attrNop0[A]("ref")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    attribute: => AttributeOp[A] = tag[AttributeTag](nop0[A]("attribute")),
    attributeGroup: => AttributeGroupOp[A] =
      tag[AttributeGroupTag](nop0[A]("attributeGroup")),
    anyAttribute: => AnyAttributeOp[A] =
      tag[AnyAttributeTag](nop0[A]("anyAttribute"))
  ): AttributeGroupOp[A] =
    attributeGroupOp(
      composeOps(
        foldAttributes(
          Eval.later(Seq("id" -> id, "name" -> name, "ref" -> ref))
        )
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
    )

  def anyAttributeOp[A](op: Op[A]): AnyAttributeOp[A] =
    tag[AnyAttributeTag][Op[A]](op)

  def anyAttribute[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    namespace: => NamespaceOp[A] = tag[NamespaceTag](attrNop0[A]("namespace")),
    processContents: => ProcessContentsOp[A] =
      tag[ProcessContentsTag](attrNop0[A]("processContents")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation"))
  ): AnyAttributeOp[A] =
    anyAttributeOp(
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
    )

  def elementOp[A](op: Op[A]): ElementOp[A] = tag[ElementTag][Op[A]](op)

  def element[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    `abstract`: => AbstractOp[A] = tag[AbstractTag](attrNop0[A]("abstract")),
    block: => BlockOp[A] = tag[BlockTag](attrNop0[A]("block")),
    default: => DefaultOp[A] = tag[DefaultTag](attrNop0[A]("default")),
    `final`: => FinalOp[A] = tag[FinalTag](attrNop0[A]("final")),
    fixed: => FixedOp[A] = tag[FixedTag](attrNop0[A]("fixed")),
    form: => FormOp[A] = tag[FormTag](attrNop0[A]("form")),
    maxOccurs: => MaxOccursOp[A] = tag[MaxOccursTag](attrNop0[A]("maxOccurs")),
    minOccurs: => MinOccursOp[A] = tag[MinOccursTag](attrNop0[A]("minOccurs")),
    name: => NameOp[A] = tag[NameTag](attrNop0[A]("name")),
    nillable: => NillableOp[A] = tag[NillableTag](attrNop0[A]("nillable")),
    ref: => RefOp[A] = tag[RefTag](attrNop0[A]("ref")),
    substitutionGroup: => SubstitutionGroupOp[A] =
      tag[SubstitutionGroupTag](attrNop0[A]("substitutionGroup")),
    `type`: => TypeOp[A] = tag[TypeTag](attrNop0[A]("type")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    simpleType: => SimpleTypeOp[A] = tag[SimpleTypeTag](nop0[A]("simpleType")),
    complexType: => ComplexTypeOp[A] =
      tag[ComplexTypeTag](nop0[A]("complexType")),
    unique: => UniqueOp[A] = tag[UniqueTag](nop0[A]("unique")),
    key: => KeyOp[A] = tag[KeyTag](nop0[A]("key")),
    keyref: => KeyrefOp[A] = tag[KeyrefTag](nop0[A]("keyref"))
  ): ElementOp[A] =
    elementOp(
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
    )

  def uniqueOp[A](op: Op[A]): UniqueOp[A] = tag[UniqueTag][Op[A]](op)

  def unique[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    name: => NameOp[A] = tag[NameTag](attrNop0[A]("name")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    selector: => SelectorOp[A] = tag[SelectorTag](nop0[A]("selector")),
    field: => FieldOp[A] = tag[FieldTag](nop0[A]("field"))
  ): UniqueOp[A] =
    uniqueOp(
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
    )

  def keyOp[A](op: Op[A]): KeyOp[A] = tag[KeyTag][Op[A]](op)

  def key[A](id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
             name: => NameOp[A] = tag[NameTag](attrNop0[A]("name")),
             annotation: => AnnotationOp[A] =
               tag[AnnotationTag](nop0[A]("annotation")),
             selector: => SelectorOp[A] = tag[SelectorTag](nop0[A]("selector")),
             field: => FieldOp[A] = tag[FieldTag](nop0[A]("field"))): KeyOp[A] =
    keyOp(
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
    )

  def keyrefOp[A](op: Op[A]): KeyrefOp[A] = tag[KeyrefTag][Op[A]](op)

  def keyref[A](
    id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
    name: => NameOp[A] = tag[NameTag](attrNop0[A]("name")),
    referer: => RefererOp[A] = tag[RefererTag](attrNop0[A]("referer")),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop0[A]("annotation")),
    selector: => SelectorOp[A] = tag[SelectorTag](nop0[A]("selector")),
    field: => FieldOp[A] = tag[FieldTag](nop0[A]("field"))
  ): KeyrefOp[A] =
    keyrefOp(
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
    )

  def selectorOp[A](op: Op[A]): SelectorOp[A] = tag[SelectorTag][Op[A]](op)

  def selector[A](id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
                  xpath: => XpathOp[A] = tag[XpathTag](attrNop0[A]("xpath")),
                  annotation: => AnnotationOp[A] =
                    tag[AnnotationTag](nop0[A]("annotation"))): SelectorOp[A] =
    selectorOp(
      composeOps(foldAttributes(Eval.later(Seq("id" -> id, "xpath" -> xpath))))(
        foldChild(Eval.later(Seq("annotation" -> annotation)))
      )
    )

  def fieldOp[A](op: Op[A]): FieldOp[A] = tag[FieldTag][Op[A]](op)

  def field[A](id: => IdOp[A] = tag[IdTag](attrNop0[A]("id")),
               xpath: => XpathOp[A] = tag[XpathTag](attrNop0[A]("xpath")),
               annotation: => AnnotationOp[A] =
                 tag[AnnotationTag](nop0[A]("annotation"))): FieldOp[A] =
    fieldOp(
      composeOps(foldAttributes(Eval.later(Seq("id" -> id, "xpath" -> xpath))))(
        foldChild(Eval.later(Seq("annotation" -> annotation)))
      )
    )

  def nop[A]: Op[A] = _ => a => ok(a)

  def attrNop[A]: AttrOp[A] = _ => a => ok(a)

  private def nop0[A](name: String): Op[A] =
    _ =>
      a =>
        for {
          strictMode <- getStrictMode
          res <- if (strictMode) error[A](s"Found unprocessed node `$name`")
          else ok(a)
        } yield res

  private def attrNop0[A](name: String): AttrOp[A] =
    _ =>
      a =>
        for {
          strictMode <- getStrictMode
          res <- if (strictMode)
            error[A](s"Found unprocessed attribute `$name`")
          else ok(a)
        } yield res

}
