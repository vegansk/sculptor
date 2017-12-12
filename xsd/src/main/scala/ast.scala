package sculptor.xsd

import cats._
import cats.data._
import cats.implicits._

object ast {

  final case class QName(name: String, ns: Option[String]) {
    override def toString(): String = {
      this match {
        case QName(n, None) => n
        case QName(n, Some(ns)) => s"$ns:$n"
      }
    }
  }

  object QName {
    def fromString(name: String): QName = name.split(":") match {
      case Array(ns, name) => QName(name, ns.some)
      case _ => QName(name, none[String])
    }

    implicit lazy val QNameEq: Eq[QName] = Eq.fromUniversalEquals[QName]
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

  final case class Annotation[F[_]](documentation: F[List[String]],
                                    appinfo: F[Option[List[String]]])
      extends AST[F]
  object Annotation extends Builder("Annotation") {
    def empty[F[_]: MonoidK](): Annotation[F] =
      apply(MonoidK[F].empty, MonoidK[F].empty)

    def build(src: Annotation[SrcF]): BuildResultId[Annotation] = {
      (
        value("documentation")(src.documentation),
        optional("appinfo")(src.appinfo)
      ).mapN(Annotation.apply[DstF])
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

  sealed trait Body[F[_]] extends AST[F]

  object Body {
    final case class element[F[_]](value: Element[F]) extends Body[F]
    final case class sequence[F[_]](value: Sequence[F]) extends Body[F]
    final case class choice[F[_]](value: Choice[F]) extends Body[F]
    final case class any[F[_]](value: Any[F]) extends Body[F]

    def fold[F[_], A](el: Element[F] => A,
                      seq: Sequence[F] => A,
                      ch: Choice[F] => A,
                      any0: Any[F] => A): Body[F] => A = _ match {
      case element(v) => el(v)
      case sequence(v) => seq(v)
      case choice(v) => ch(v)
      case any(v) => any0(v)
    }

    def build: Body[SrcF] => BuildResultId[Body] = fold(
      Element.build(_).map(element(_): Body[DstF]),
      Sequence.build(_).map(sequence(_)),
      Choice.build(_).map(choice(_)),
      Any.build(_).map(any(_))
    )
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

  sealed trait Type[F[_]]

  object Type {
    final case class simpleType[F[_]](value: SimpleType[F]) extends Type[F]
    final case class complexType[F[_]](value: ComplexType[F]) extends Type[F]
    final case class element[F[_]](value: Element[F]) extends Type[F]

    def fold[F[_], A](st: SimpleType[F] => A,
                      ct: ComplexType[F] => A,
                      el: Element[F] => A): Type[F] => A = _ match {
      case simpleType(v) => st(v)
      case complexType(v) => ct(v)
      case element(v) => el(v)
    }

    def build: Type[SrcF] => BuildResultId[Type] = fold(
      SimpleType.build(_).map(simpleType(_): Type[DstF]),
      ComplexType.build(_).map(complexType(_)),
      Element.build(_).map(element(_))
    )
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
