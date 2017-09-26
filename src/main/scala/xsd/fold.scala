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

  type SchemaOp[A] = Op[A] @@ SchemaTag
  type IdOp[A] = AttrOp[A] @@ IdTag
  type TargetNamespaceOp[A] = AttrOp[A] @@ TargetNamespaceTag
  type FinalOp[A] = AttrOp[A] @@ FinalTag
  type NameOp[A] = AttrOp[A] @@ NameTag
  type ItemTypeOp[A] = AttrOp[A] @@ ItemTypeTag
  type BaseOp[A] = AttrOp[A] @@ BaseTag
  type MemberTypesOp[A] = AttrOp[A] @@ MemberTypesTag
  type AbstractOp[A] = AttrOp[A] @@ AbstractTag
  type BlockOp[A] = AttrOp[A] @@ BlockTag
  type MixedOp[A] = AttrOp[A] @@ MixedTag
  type MaxOccursOp[A] = AttrOp[A] @@ MaxOccursTag
  type MinOccursOp[A] = AttrOp[A] @@ MinOccursTag
  type RefOp[A] = AttrOp[A] @@ RefTag
  type NamespaceOp[A] = AttrOp[A] @@ NamespaceTag
  type ProcessContentsOp[A] = AttrOp[A] @@ ProcessContentsTag
  type DefaultOp[A] = AttrOp[A] @@ DefaultTag
  type FixedOp[A] = AttrOp[A] @@ FixedTag
  type FormOp[A] = AttrOp[A] @@ FormTag
  type TypeOp[A] = AttrOp[A] @@ TypeTag
  type UseOp[A] = AttrOp[A] @@ UseTag
  type NillableOp[A] = AttrOp[A] @@ NillableTag
  type SubstitutionGroupOp[A] = AttrOp[A] @@ SubstitutionGroupTag
  type RefererOp[A] = AttrOp[A] @@ RefererTag
  type XpathOp[A] = AttrOp[A] @@ XpathTag

  type AnnotationOp[A] = Op[A] @@ AnnotationTag
  type SimpleTypeOp[A] = Op[A] @@ SimpleTypeTag
  type ComplexTypeOp[A] = Op[A] @@ ComplexTypeTag
  type ElementOp[A] = Op[A] @@ ElementTag
  type AttributeOp[A] = Op[A] @@ AttributeTag
  type AppinfoOp[A] = Op[A] @@ AppinfoTag
  type DocumentationOp[A] = Op[A] @@ DocumentationTag
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
    )

  def schema[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    targetNamespace: => TargetNamespaceOp[A] =
      tag[TargetNamespaceTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
    simpleType: => SimpleTypeOp[A] = tag[SimpleTypeTag](nop[A]),
    complexType: => ComplexTypeOp[A] = tag[ComplexTypeTag](nop[A]),
    element: => ElementOp[A] = tag[ElementTag](nop[A]),
    attribute: => AttributeOp[A] = tag[AttributeTag](nop[A])
  ): SchemaOp[A] =
    tag[SchemaTag][Op[A]](
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
    )

  def annotation[A](id: => IdOp[A] = tag[IdTag](attrNop[A]),
                    appinfo: => AppinfoOp[A] = tag[AppinfoTag](nop[A]),
                    documentation: => DocumentationOp[A] =
                      tag[DocumentationTag](nop[A])): AnnotationOp[A] =
    tag[AnnotationTag][Op[A]](
      composeOps(foldAttributes(Eval.later(Seq("id" -> id))))(
        foldChild(
          Eval
            .later(Seq("appinfo" -> appinfo, "documentation" -> documentation))
        )
      )
    )

  def simpleType[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    `final`: => FinalOp[A] = tag[FinalTag](attrNop[A]),
    name: => NameOp[A] = tag[NameTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
    list: => ListOp[A] = tag[ListTag](nop[A]),
    restriction: => SimpleTypeRestrictionOp[A] =
      tag[SimpleTypeRestrictionTag](nop[A]),
    union: => UnionOp[A] = tag[UnionTag](nop[A])
  ): SimpleTypeOp[A] =
    tag[SimpleTypeTag][Op[A]](
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

  def list[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    itemType: => ItemTypeOp[A] = tag[ItemTypeTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
    simpleType: => SimpleTypeOp[A] = tag[SimpleTypeTag](nop[A])
  ): ListOp[A] =
    tag[ListTag][Op[A]](
      composeOps(
        foldAttributes(Eval.later(Seq("id" -> id, "itemType" -> itemType)))
      )(
        foldChild(
          Eval
            .later(Seq("annotation" -> annotation, "simpleType" -> simpleType))
        )
      )
    )

  def simpleTypeRestriction[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    base: => BaseOp[A] = tag[BaseTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
    simpleType: => SimpleTypeOp[A] = tag[SimpleTypeTag](nop[A]),
    minExclusive: => MinExclusiveOp[A] = tag[MinExclusiveTag](nop[A]),
    minInclusive: => MinInclusiveOp[A] = tag[MinInclusiveTag](nop[A]),
    maxExclusive: => MaxExclusiveOp[A] = tag[MaxExclusiveTag](nop[A]),
    maxInclusive: => MaxInclusiveOp[A] = tag[MaxInclusiveTag](nop[A]),
    totalDigits: => TotalDigitsOp[A] = tag[TotalDigitsTag](nop[A]),
    fractionDigits: => FractionDigitsOp[A] = tag[FractionDigitsTag](nop[A]),
    length: => LengthOp[A] = tag[LengthTag](nop[A]),
    minLength: => MinLengthOp[A] = tag[MinLengthTag](nop[A]),
    maxLength: => MaxLengthOp[A] = tag[MaxLengthTag](nop[A]),
    enumeration: => EnumerationOp[A] = tag[EnumerationTag](nop[A]),
    whiteSpace: => WhiteSpaceOp[A] = tag[WhiteSpaceTag](nop[A]),
    pattern: => PatternOp[A] = tag[PatternTag](nop[A])
  ): SimpleTypeRestrictionOp[A] =
    tag[SimpleTypeRestrictionTag][Op[A]](
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

  def union[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    memberTypes: => MemberTypesOp[A] = tag[MemberTypesTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
    simpleType: => SimpleTypeOp[A] = tag[SimpleTypeTag](nop[A])
  ): UnionOp[A] =
    tag[UnionTag][Op[A]](
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

  def complexType[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    `abstract`: => AbstractOp[A] = tag[AbstractTag](attrNop[A]),
    block: => BlockOp[A] = tag[BlockTag](attrNop[A]),
    `final`: => FinalOp[A] = tag[FinalTag](attrNop[A]),
    mixed: => MixedOp[A] = tag[MixedTag](attrNop[A]),
    name: => NameOp[A] = tag[NameTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
    simpleContent: => SimpleContentOp[A] = tag[SimpleContentTag](nop[A]),
    complexContent: => ComplexContentOp[A] = tag[ComplexContentTag](nop[A]),
    group: => GroupOp[A] = tag[GroupTag](nop[A]),
    all: => AllOp[A] = tag[AllTag](nop[A]),
    choice: => ChoiceOp[A] = tag[ChoiceTag](nop[A]),
    sequence: => SequenceOp[A] = tag[SequenceTag](nop[A]),
    attribute: => AttributeOp[A] = tag[AttributeTag](nop[A])
  ): ComplexTypeOp[A] =
    tag[ComplexTypeTag][Op[A]](
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

  def complexContent[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    mixed: => MixedOp[A] = tag[MixedTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
    restriction: => ComplexContentRestrictionOp[A] =
      tag[ComplexContentRestrictionTag](nop[A]),
    extension: => ComplexContentExtensionOp[A] =
      tag[ComplexContentExtensionTag](nop[A])
  ): ComplexContentOp[A] =
    tag[ComplexContentTag][Op[A]](
      composeOps(
        foldAttributes(Eval.later(Seq("id" -> id, "mixed" -> mixed)))
      )(
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

  def complexContentRestriction[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    base: => BaseOp[A] = tag[BaseTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
    group: => GroupOp[A] = tag[GroupTag](nop[A]),
    all: => AllOp[A] = tag[AllTag](nop[A]),
    choice: => ChoiceOp[A] = tag[ChoiceTag](nop[A]),
    sequence: => SequenceOp[A] = tag[SequenceTag](nop[A]),
    attribute: => AttributeOp[A] = tag[AttributeTag](nop[A]),
    attributeGroup: => AttributeGroupOp[A] = tag[AttributeGroupTag](nop[A]),
    anyAttribute: => AnyAttributeOp[A] = tag[AnyAttributeTag](nop[A])
  ): ComplexContentRestrictionOp[A] =
    tag[ComplexContentRestrictionTag][Op[A]](
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

  def complexContentExtension[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    base: => BaseOp[A] = tag[BaseTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
    group: => GroupOp[A] = tag[GroupTag](nop[A]),
    all: => AllOp[A] = tag[AllTag](nop[A]),
    choice: => ChoiceOp[A] = tag[ChoiceTag](nop[A]),
    sequence: => SequenceOp[A] = tag[SequenceTag](nop[A]),
    attribute: => AttributeOp[A] = tag[AttributeTag](nop[A]),
    attributeGroup: => AttributeGroupOp[A] = tag[AttributeGroupTag](nop[A]),
    anyAttribute: => AnyAttributeOp[A] = tag[AnyAttributeTag](nop[A])
  ): ComplexContentExtensionOp[A] =
    tag[ComplexContentExtensionTag][Op[A]](
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

  def group[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    name: => NameOp[A] = tag[NameTag](attrNop[A]),
    maxOccurs: => MaxOccursOp[A] = tag[MaxOccursTag](attrNop[A]),
    minOccurs: => MinOccursOp[A] = tag[MinOccursTag](attrNop[A]),
    ref: => RefOp[A] = tag[RefTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
    all: => AllOp[A] = tag[AllTag](nop[A]),
    choice: => ChoiceOp[A] = tag[ChoiceTag](nop[A]),
    sequence: => SequenceOp[A] = tag[SequenceTag](nop[A])
  ): GroupOp[A] =
    tag[GroupTag][Op[A]](
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

  def all[A](id: => IdOp[A] = tag[IdTag](attrNop[A]),
             maxOccurs: => MaxOccursOp[A] = tag[MaxOccursTag](attrNop[A]),
             minOccurs: => MinOccursOp[A] = tag[MinOccursTag](attrNop[A]),
             annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
             element: => ElementOp[A] = tag[ElementTag](nop[A])): AllOp[A] =
    tag[AllTag][Op[A]](
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

  def choice[A](id: => IdOp[A] = tag[IdTag](attrNop[A]),
                maxOccurs: => MaxOccursOp[A] = tag[MaxOccursTag](attrNop[A]),
                minOccurs: => MinOccursOp[A] = tag[MinOccursTag](attrNop[A]),
                annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
                element: => ElementOp[A] = tag[ElementTag](nop[A]),
                group: => GroupOp[A] = tag[GroupTag](nop[A]),
                choice: => ChoiceOp[A] = tag[ChoiceTag](nop[A]),
                sequence: => SequenceOp[A] = tag[SequenceTag](nop[A]),
                any: => AnyOp[A] = tag[AnyTag](nop[A])): ChoiceOp[A] =
    tag[ChoiceTag][Op[A]](
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

  def sequence[A](id: => IdOp[A] = tag[IdTag](attrNop[A]),
                  maxOccurs: => MaxOccursOp[A] = tag[MaxOccursTag](attrNop[A]),
                  minOccurs: => MinOccursOp[A] = tag[MinOccursTag](attrNop[A]),
                  annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
                  element: => ElementOp[A] = tag[ElementTag](nop[A]),
                  group: => GroupOp[A] = tag[GroupTag](nop[A]),
                  choice: => ChoiceOp[A] = tag[ChoiceTag](nop[A]),
                  sequence: => SequenceOp[A] = tag[SequenceTag](nop[A]),
                  any: => AnyOp[A] = tag[AnyTag](nop[A])): SequenceOp[A] =
    tag[SequenceTag][Op[A]](
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

  def any[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    maxOccurs: => MaxOccursOp[A] = tag[MaxOccursTag](attrNop[A]),
    minOccurs: => MinOccursOp[A] = tag[MinOccursTag](attrNop[A]),
    namespace: => NamespaceOp[A] = tag[NamespaceTag](attrNop[A]),
    processContents: => ProcessContentsOp[A] =
      tag[ProcessContentsTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A])
  ): AnyOp[A] =
    tag[AnyTag][Op[A]](
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

  def simpleContent[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
    restriction: => SimpleContentRestrictionOp[A] =
      tag[SimpleContentRestrictionTag](nop[A]),
    extension: => SimpleContentExtensionOp[A] =
      tag[SimpleContentExtensionTag](nop[A])
  ): SimpleContentOp[A] =
    tag[SimpleContentTag][Op[A]](
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

  def simpleContentRestriction[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    base: => BaseOp[A] = tag[BaseTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
    simpleType: => SimpleTypeOp[A] = tag[SimpleTypeTag](nop[A]),
    minExclusive: => MinExclusiveOp[A] = tag[MinExclusiveTag](nop[A]),
    minInclusive: => MinInclusiveOp[A] = tag[MinInclusiveTag](nop[A]),
    maxExclusive: => MaxExclusiveOp[A] = tag[MaxExclusiveTag](nop[A]),
    maxInclusive: => MaxInclusiveOp[A] = tag[MaxInclusiveTag](nop[A]),
    totalDigits: => TotalDigitsOp[A] = tag[TotalDigitsTag](nop[A]),
    fractionDigits: => FractionDigitsOp[A] = tag[FractionDigitsTag](nop[A]),
    length: => LengthOp[A] = tag[LengthTag](nop[A]),
    minLength: => MinLengthOp[A] = tag[MinLengthTag](nop[A]),
    maxLength: => MaxLengthOp[A] = tag[MaxLengthTag](nop[A]),
    enumeration: => EnumerationOp[A] = tag[EnumerationTag](nop[A]),
    whiteSpace: => WhiteSpaceOp[A] = tag[WhiteSpaceTag](nop[A]),
    pattern: => PatternOp[A] = tag[PatternTag](nop[A]),
    attribute: => AttributeOp[A] = tag[AttributeTag](nop[A]),
    attributeGroup: => AttributeGroupOp[A] = tag[AttributeGroupTag](nop[A]),
    anyAttribute: => AnyAttributeOp[A] = tag[AnyAttributeTag](nop[A])
  ): SimpleContentRestrictionOp[A] =
    tag[SimpleContentRestrictionTag][Op[A]](
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

  def simpleContentExtension[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    base: => BaseOp[A] = tag[BaseTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
    attribute: => AttributeOp[A] = tag[AttributeTag](nop[A]),
    attributeGroup: => AttributeGroupOp[A] = tag[AttributeGroupTag](nop[A]),
    anyAttribute: => AnyAttributeOp[A] = tag[AnyAttributeTag](nop[A])
  ): SimpleContentExtensionOp[A] =
    tag[SimpleContentExtensionTag][Op[A]](
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

  def attribute[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    default: => DefaultOp[A] = tag[DefaultTag](attrNop[A]),
    fixed: => FixedOp[A] = tag[FixedTag](attrNop[A]),
    form: => FormOp[A] = tag[FormTag](attrNop[A]),
    name: => NameOp[A] = tag[NameTag](attrNop[A]),
    ref: => RefOp[A] = tag[RefTag](attrNop[A]),
    `type`: => TypeOp[A] = tag[TypeTag](attrNop[A]),
    use: => UseOp[A] = tag[UseTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
    simpleType: => SimpleTypeOp[A] = tag[SimpleTypeTag](nop[A])
  ): AttributeOp[A] =
    tag[AttributeTag][Op[A]](
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

  def attributeGroup[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    name: => NameOp[A] = tag[NameTag](attrNop[A]),
    ref: => RefOp[A] = tag[RefTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
    attribute: => AttributeOp[A] = tag[AttributeTag](nop[A]),
    attributeGroup: => AttributeGroupOp[A] = tag[AttributeGroupTag](nop[A]),
    anyAttribute: => AnyAttributeOp[A] = tag[AnyAttributeTag](nop[A])
  ): AttributeGroupOp[A] =
    tag[AttributeGroupTag][Op[A]](
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

  def anyAttribute[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    namespace: => NamespaceOp[A] = tag[NamespaceTag](attrNop[A]),
    processContents: => ProcessContentsOp[A] =
      tag[ProcessContentsTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A])
  ): AnyAttributeOp[A] =
    tag[AnyAttributeTag][Op[A]](
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

  def element[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    `abstract`: => AbstractOp[A] = tag[AbstractTag](attrNop[A]),
    block: => BlockOp[A] = tag[BlockTag](attrNop[A]),
    default: => DefaultOp[A] = tag[DefaultTag](attrNop[A]),
    `final`: => FinalOp[A] = tag[FinalTag](attrNop[A]),
    fixed: => FixedOp[A] = tag[FixedTag](attrNop[A]),
    form: => FormOp[A] = tag[FormTag](attrNop[A]),
    maxOccurs: => MaxOccursOp[A] = tag[MaxOccursTag](attrNop[A]),
    minOccurs: => MinOccursOp[A] = tag[MinOccursTag](attrNop[A]),
    name: => NameOp[A] = tag[NameTag](attrNop[A]),
    nillable: => NillableOp[A] = tag[NillableTag](attrNop[A]),
    ref: => RefOp[A] = tag[RefTag](attrNop[A]),
    substitutionGroup: => SubstitutionGroupOp[A] =
      tag[SubstitutionGroupTag](attrNop[A]),
    `type`: => TypeOp[A] = tag[TypeTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
    simpleType: => SimpleTypeOp[A] = tag[SimpleTypeTag](nop[A]),
    complexType: => ComplexTypeOp[A] = tag[ComplexTypeTag](nop[A]),
    unique: => UniqueOp[A] = tag[UniqueTag](nop[A]),
    key: => KeyOp[A] = tag[KeyTag](nop[A]),
    keyref: => KeyrefOp[A] = tag[KeyrefTag](nop[A])
  ): ElementOp[A] =
    tag[ElementTag][Op[A]](
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

  def unique[A](id: => IdOp[A] = tag[IdTag](attrNop[A]),
                name: => NameOp[A] = tag[NameTag](attrNop[A]),
                annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
                selector: => SelectorOp[A] = tag[SelectorTag](nop[A]),
                field: => FieldOp[A] = tag[FieldTag](nop[A])): UniqueOp[A] =
    tag[UniqueTag][Op[A]](
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

  def key[A](id: => IdOp[A] = tag[IdTag](attrNop[A]),
             name: => NameOp[A] = tag[NameTag](attrNop[A]),
             annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
             selector: => SelectorOp[A] = tag[SelectorTag](nop[A]),
             field: => FieldOp[A] = tag[FieldTag](nop[A])): KeyOp[A] =
    tag[KeyTag][Op[A]](
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

  def keyref[A](id: => IdOp[A] = tag[IdTag](attrNop[A]),
                name: => NameOp[A] = tag[NameTag](attrNop[A]),
                referer: => RefererOp[A] = tag[RefererTag](attrNop[A]),
                annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A]),
                selector: => SelectorOp[A] = tag[SelectorTag](nop[A]),
                field: => FieldOp[A] = tag[FieldTag](nop[A])): KeyrefOp[A] =
    tag[KeyrefTag][Op[A]](
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

  def selector[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    xpath: => XpathOp[A] = tag[XpathTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A])
  ): SelectorOp[A] =
    tag[SelectorTag][Op[A]](
      composeOps(
        foldAttributes(Eval.later(Seq("id" -> id, "xpath" -> xpath)))
      )(foldChild(Eval.later(Seq("annotation" -> annotation))))
    )

  def field[A](
    id: => IdOp[A] = tag[IdTag](attrNop[A]),
    xpath: => XpathOp[A] = tag[XpathTag](attrNop[A]),
    annotation: => AnnotationOp[A] = tag[AnnotationTag](nop[A])
  ): FieldOp[A] =
    tag[FieldTag][Op[A]](
      composeOps(
        foldAttributes(Eval.later(Seq("id" -> id, "xpath" -> xpath)))
      )(foldChild(Eval.later(Seq("annotation" -> annotation))))
    )

  def nop[A]: Op[A] = a => _ => ok(a)

  def attrNop[A]: AttrOp[A] = a => _ => ok(a)
}
