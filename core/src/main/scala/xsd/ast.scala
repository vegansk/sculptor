package sculptor.xsd

import cats._
import cats.data._
import cats.implicits._
import shapeless.{Id => _, _}

object ast {

  final case class QName(name: String, ns: Option[String])
  object QName {
    def fromString(name: String): QName = name.split(":") match {
      case Array(name, ns) => QName(name, ns.some)
      case _ => QName(name, none[String])
    }
  }

  type SrcF[A] = Option[A]
  type DstF[A] = Id[A]
  type BuildResult[A] = ValidatedNel[String, A]
  type BuildResultId[F[_[_]]] = ValidatedNel[String, DstF[F[DstF]]]
  type BuildResultOption[F[_[_]]] = ValidatedNel[String, DstF[Option[F[DstF]]]]
  type BuildResultList[F[_[_]]] = ValidatedNel[String, DstF[List[F[DstF]]]]

  abstract class Builder(name: String) {

    def value[A](fName: String)(a: SrcF[A]): BuildResult[A] =
      a.toValidNel(s"Required field $name.$fName not set")
    def optional[A](
      fName: String
    )(a: SrcF[Option[A]]): BuildResult[Option[A]] =
      Validated.valid(a.flatten)
    def innerOptional[F[_[_]]](fName: String, f: F[SrcF] => BuildResultId[F])(
      a: SrcF[Option[F[SrcF]]]
    ): BuildResultOption[F] =
      a.flatten.traverse(f)
    def innerList[F[_[_]]](fName: String, f: F[SrcF] => BuildResultId[F])(
      a: SrcF[List[F[SrcF]]]
    ): BuildResultList[F] =
      a.toList.flatten.traverse(f)
  }

  sealed trait AST[F[_]]

  final case class Annotation[F[_]](documentation: F[List[String]])
      extends AST[F]
  object Annotation extends Builder("Annotation") {
    def empty[F[_]: MonoidK](): Annotation[F] =
      apply(MonoidK[F].empty)

    def build(src: Annotation[SrcF]): BuildResultId[Annotation] = {
      (
        value("documentation")(src.documentation) :: HNil
      ).tupled.map(Annotation.apply[DstF])
    }
  }

  final case class Enumeration[F[_]](value: F[String],
                                     annotation: F[Option[Annotation[F]]])
      extends AST[F]

  object Enumeration extends Builder("Enumeration") {
    def empty[F[_]: MonoidK](): Enumeration[F] =
      apply(MonoidK[F].empty, MonoidK[F].empty)

    def build(src: Enumeration[SrcF]): BuildResultId[Enumeration] = {
      (
        value("value")(src.value),
        innerOptional("annotation", Annotation.build)(src.annotation)
      ).mapN(Enumeration.apply[DstF])
    }
  }

  final case class SimpleTypeRestriction[F[_]](
    base: F[QName],
    pattern: F[Option[String]],
    minLength: F[Option[String]],
    maxLength: F[Option[String]],
    enumeration: F[List[Enumeration[F]]]
  ) extends AST[F]

  object SimpleTypeRestriction extends Builder("SimpleTypeRestriction") {
    def empty[F[_]: MonoidK](): SimpleTypeRestriction[F] =
      apply(
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty
      )

    def build(
      src: SimpleTypeRestriction[SrcF]
    ): BuildResultId[SimpleTypeRestriction] = {
      (
        value("base")(src.base),
        optional("pattern")(src.pattern),
        optional("minLength")(src.minLength),
        optional("maxLength")(src.maxLength),
        innerList("enumeration", Enumeration.build)(src.enumeration)
      ).mapN(SimpleTypeRestriction.apply[DstF])
    }
  }

  final case class SimpleType[F[_]](
    annotation: F[Option[Annotation[F]]],
    name: F[Option[String]],
    `final`: F[Option[String]],
    restriction: F[Option[SimpleTypeRestriction[F]]]
  ) extends AST[F]

  object SimpleType extends Builder("SimpleType") {
    def empty[F[_]: MonoidK](): SimpleType[F] =
      apply(
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty
      )

    def build(src: SimpleType[SrcF]): BuildResultId[SimpleType] = {
      (
        innerOptional("annotation", Annotation.build)(src.annotation),
        optional("name")(src.name),
        optional("final")(src.`final`),
        innerOptional("restriction", SimpleTypeRestriction.build)(
          src.restriction
        )
      ).mapN(SimpleType.apply[DstF])
    }
  }

  final case class Attribute[F[_]](annotation: F[Option[Annotation[F]]],
                                   name: F[Option[String]],
                                   form: F[Option[String]],
                                   fixed: F[Option[String]],
                                   `type`: F[Option[String]],
                                   use: F[Option[String]])
      extends AST[F]

  object Attribute extends Builder("Attribute") {
    def empty[F[_]: MonoidK](): Attribute[F] =
      apply(
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty
      )

    def build(src: Attribute[SrcF]): BuildResultId[Attribute] = {
      (
        innerOptional("annotation", Annotation.build)(src.annotation),
        optional("name")(src.name),
        optional("form")(src.form),
        optional("fixed")(src.fixed),
        optional("type")(src.`type`),
        optional("use")(src.use)
      ).mapN(Attribute.apply[DstF])
    }
  }

  type Body[F[_]] =
    Element[F] :+: Sequence[F] :+: Choice[F] :+: Any[F] :+: CNil

  object Body {
    object buildImpl extends Poly1 {
      implicit val element: Case.Aux[Element[SrcF], BuildResultId[Body]] = at(
        Element.build(_).map(Coproduct[Body[DstF]](_))
      )
      implicit val sequence: Case.Aux[Sequence[SrcF], BuildResultId[Body]] =
        at(Sequence.build(_).map(Coproduct[Body[DstF]](_)))
      implicit val choice: Case.Aux[Choice[SrcF], BuildResultId[Body]] = at(
        Choice.build(_).map(Coproduct[Body[DstF]](_))
      )
      implicit val any: Case.Aux[Any[SrcF], BuildResultId[Body]] = at(
        Any.build(_).map(Coproduct[Body[DstF]](_))
      )
    }

    def build(src: Body[SrcF]): BuildResultId[Body] = src.fold(buildImpl)
  }

  final case class Sequence[F[_]](annotation: F[Option[Annotation[F]]],
                                  body: F[List[Body[F]]],
                                  minOccurs: F[Option[String]],
                                  maxOccurs: F[Option[String]])
      extends AST[F]

  object Sequence extends Builder("Sequence") {
    def empty[F[_]: MonoidK](): Sequence[F] =
      apply(
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty
      )

    def build(src: Sequence[SrcF]): BuildResultId[Sequence] = {
      (
        innerOptional("annotation", Annotation.build)(src.annotation),
        innerList("body", Body.build)(src.body),
        optional("minOccurs")(src.minOccurs),
        optional("maxOccurs")(src.maxOccurs)
      ).mapN(Sequence.apply[DstF])
    }
  }

  final case class Choice[F[_]](annotation: F[Option[Annotation[F]]],
                                body: F[List[Body[F]]],
                                minOccurs: F[Option[String]],
                                maxOccurs: F[Option[String]])
      extends AST[F]

  object Choice extends Builder("Choice") {
    def empty[F[_]: MonoidK](): Choice[F] =
      apply(
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty
      )

    def build(src: Choice[SrcF]): BuildResultId[Choice] = {
      (
        innerOptional("annotation", Annotation.build)(src.annotation),
        innerList("body", Body.build)(src.body),
        optional("minOccurs")(src.minOccurs),
        optional("maxOccurs")(src.maxOccurs)
      ).mapN(Choice.apply[DstF])
    }
  }

  final case class Any[F[_]](annotation: F[Option[Annotation[F]]],
                             processContents: F[Option[String]],
                             minOccurs: F[Option[String]],
                             maxOccurs: F[Option[String]])
      extends AST[F]

  object Any extends Builder("Any") {
    def empty[F[_]: MonoidK](): Any[F] =
      apply(
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty
      )

    def build(src: Any[SrcF]): BuildResultId[Any] = {
      (
        innerOptional("annotation", Annotation.build)(src.annotation),
        optional("processContents")(src.processContents),
        optional("minOccurs")(src.minOccurs),
        optional("maxOccurs")(src.maxOccurs)
      ).mapN(Any.apply[DstF])
    }
  }

  final case class ComplexContentExtension[F[_]](
    annotation: F[Option[Annotation[F]]],
    base: F[QName],
    sequence: F[Option[Sequence[F]]]
  ) extends AST[F]

  object ComplexContentExtension extends Builder("ComplexContentExtension") {
    def empty[F[_]: MonoidK](): ComplexContentExtension[F] =
      apply(MonoidK[F].empty, MonoidK[F].empty, MonoidK[F].empty)

    def build(
      src: ComplexContentExtension[SrcF]
    ): BuildResultId[ComplexContentExtension] = {
      (
        innerOptional("annotation", Annotation.build)(src.annotation),
        value("base")(src.base),
        innerOptional("sequence", Sequence.build)(src.sequence)
      ).mapN(ComplexContentExtension.apply[DstF])
    }
  }

  final case class ComplexContent[F[_]](
    annotation: F[Option[Annotation[F]]],
    extension: F[Option[ComplexContentExtension[F]]]
  ) extends AST[F]

  object ComplexContent extends Builder("ComplexContent") {
    def empty[F[_]: MonoidK](): ComplexContent[F] =
      apply(MonoidK[F].empty, MonoidK[F].empty)

    def build(src: ComplexContent[SrcF]): BuildResultId[ComplexContent] = {
      (
        innerOptional("annotation", Annotation.build)(src.annotation),
        innerOptional("extension", ComplexContentExtension.build)(
          src.extension
        )
      ).mapN(ComplexContent.apply[DstF])
    }
  }

  final case class ComplexType[F[_]](
    annotation: F[Option[Annotation[F]]],
    name: F[Option[String]],
    complexContent: F[Option[ComplexContent[F]]],
    sequence: F[Option[Sequence[F]]],
    choice: F[Option[Choice[F]]],
    attributes: F[List[Attribute[F]]],
    mixed: F[Option[String]],
    block: F[Option[String]],
    `abstract`: F[Option[String]],
    `final`: F[Option[String]]
  ) extends AST[F]

  object ComplexType extends Builder("ComplexType") {
    def empty[F[_]: MonoidK](): ComplexType[F] =
      apply(
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty
      )

    def build(src: ComplexType[SrcF]): BuildResultId[ComplexType] = {
      (
        innerOptional("annotation", Annotation.build)(src.annotation),
        optional("name")(src.name),
        innerOptional("complexContent", ComplexContent.build)(
          src.complexContent
        ),
        innerOptional("sequence", Sequence.build)(src.sequence),
        innerOptional("choice", Choice.build)(src.choice),
        innerList("attributes", Attribute.build)(src.attributes),
        optional("mixed")(src.mixed),
        optional("block")(src.block),
        optional("abstract")(src.`abstract`),
        optional("final")(src.`final`)
      ).mapN(ComplexType.apply[DstF])
    }
  }

  final case class Element[F[_]](annotation: F[Option[Annotation[F]]],
                                 name: F[Option[String]],
                                 complexType: F[Option[ComplexType[F]]],
                                 simpleType: F[Option[SimpleType[F]]],
                                 `type`: F[Option[String]],
                                 minOccurs: F[Option[String]],
                                 maxOccurs: F[Option[String]],
                                 nillable: F[Option[String]])
      extends AST[F]

  object Element extends Builder("Element") {
    def empty[F[_]: MonoidK](): Element[F] =
      apply(
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty
      )

    def build(src: Element[SrcF]): BuildResultId[Element] = {
      (
        innerOptional("annotation", Annotation.build)(src.annotation),
        optional("name")(src.name),
        innerOptional("complexType", ComplexType.build)(src.complexType),
        innerOptional("simpleType", SimpleType.build)(src.simpleType),
        optional("type")(src.`type`),
        optional("minOccurs")(src.minOccurs),
        optional("maxOccurs")(src.maxOccurs),
        optional("nillable")(src.nillable)
      ).mapN(Element.apply[DstF])
    }
  }

  type Type[F[_]] = SimpleType[F] :+: ComplexType[F] :+: Element[F] :+: CNil

  object Type {
    object buildImpl extends Poly1 {
      implicit val simpleType
        : Case.Aux[SimpleType[SrcF], BuildResultId[Type]] = at(
        SimpleType.build(_).map(Coproduct[Type[DstF]](_))
      )
      implicit val complexType
        : Case.Aux[ComplexType[SrcF], BuildResultId[Type]] = at(
        ComplexType.build(_).map(Coproduct[Type[DstF]](_))
      )
      implicit val element: Case.Aux[Element[SrcF], BuildResultId[Type]] = at(
        Element.build(_).map(Coproduct[Type[DstF]](_))
      )
    }

    def build(src: Type[SrcF]): BuildResultId[Type] = src.fold(buildImpl)
  }

  final case class Schema[F[_]](annotation: F[Option[Annotation[F]]],
                                types: F[List[Type[F]]])
      extends AST[F]

  object Schema extends Builder("Schema") {
    def empty[F[_]: MonoidK](): Schema[F] =
      apply(MonoidK[F].empty, MonoidK[F].empty)

    def build(src: Schema[SrcF]): BuildResultId[Schema] = {
      (
        innerOptional("annotation", Annotation.build)(src.annotation),
        innerList("types", Type.build)(src.types)
      ).mapN(Schema.apply[DstF])
    }
  }

}
