package sculptor.xsd

import cats._
import shapeless._

object ast {

  sealed trait AST[F[_]]

  final case class Annotation[F[_]](documentation: F[List[String]])
      extends AST[F]
  object Annotation {
    def empty[F[_]: MonoidK](): Annotation[F] =
      apply(MonoidK[F].empty)
  }

  final case class Enumeration[F[_]](value: F[String],
                                     annotation: F[Option[Annotation[F]]])
      extends AST[F]

  object Enumeration {
    def empty[F[_]: MonoidK](): Enumeration[F] =
      apply(MonoidK[F].empty, MonoidK[F].empty)
  }

  final case class SimpleTypeRestriction[F[_]](
    base: F[String],
    pattern: F[Option[String]],
    minLength: F[Option[String]],
    maxLength: F[Option[String]],
    enumeration: F[List[Enumeration[F]]]
  ) extends AST[F]

  object SimpleTypeRestriction {
    def empty[F[_]: MonoidK](): SimpleTypeRestriction[F] =
      apply(
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty
      )
  }

  final case class SimpleType[F[_]](
    annotation: F[Option[Annotation[F]]],
    name: F[Option[String]],
    `final`: F[Option[String]],
    restriction: F[Option[SimpleTypeRestriction[F]]]
  ) extends AST[F]

  object SimpleType {
    def empty[F[_]: MonoidK](): SimpleType[F] =
      apply(
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty
      )
  }

  final case class Attribute[F[_]](annotation: F[Option[Annotation[F]]],
                                   name: F[Option[String]],
                                   form: F[Option[String]],
                                   fixed: F[Option[String]],
                                   `type`: F[Option[String]],
                                   use: F[Option[String]])
      extends AST[F]

  object Attribute {
    def empty[F[_]: MonoidK](): Attribute[F] =
      apply(
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty
      )
  }

  type Body[F[_]] =
    Element[F] :+: Sequence[F] :+: Choice[F] :+: Any[F] :+: CNil

  final case class Sequence[F[_]](annotation: F[Option[Annotation[F]]],
                                  body: F[List[Body[F]]],
                                  minOccurs: F[Option[String]],
                                  maxOccurs: F[Option[String]])
      extends AST[F]

  object Sequence {
    def empty[F[_]: MonoidK](): Sequence[F] =
      apply(
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty
      )
  }

  final case class Choice[F[_]](annotation: F[Option[Annotation[F]]],
                                body: F[List[Body[F]]],
                                minOccurs: F[Option[String]],
                                maxOccurs: F[Option[String]])
      extends AST[F]

  object Choice {
    def empty[F[_]: MonoidK](): Choice[F] =
      apply(
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty
      )
  }

  final case class Any[F[_]](annotation: F[Option[Annotation[F]]],
                             processContents: F[Option[String]],
                             minOccurs: F[Option[String]],
                             maxOccurs: F[Option[String]])
      extends AST[F]

  object Any {
    def empty[F[_]: MonoidK](): Any[F] =
      apply(
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty,
        MonoidK[F].empty
      )
  }

  final case class ComplexContentExtension[F[_]](
    annotation: F[Option[Annotation[F]]],
    base: F[String],
    sequence: F[Option[Sequence[F]]]
  ) extends AST[F]

  object ComplexContentExtension {
    def empty[F[_]: MonoidK](): ComplexContentExtension[F] =
      apply(MonoidK[F].empty, MonoidK[F].empty, MonoidK[F].empty)
  }

  final case class ComplexContent[F[_]](
    annotation: F[Option[Annotation[F]]],
    extension: F[Option[ComplexContentExtension[F]]]
  ) extends AST[F]

  object ComplexContent {
    def empty[F[_]: MonoidK](): ComplexContent[F] =
      apply(MonoidK[F].empty, MonoidK[F].empty)
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

  object ComplexType {
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

  object Element {
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
  }

  final case class Type[F[_]](`type`: Type.AST[F]) extends AST[F]

  object Type {
    sealed trait AST[F[_]]

    final case class Simple[F[_]](`type`: SimpleType[F]) extends AST[F]
    final case class Complex[F[_]](`type`: ComplexType[F]) extends AST[F]
    final case class Elem[F[_]](`type`: Element[F]) extends AST[F]
  }

  final case class Schema[F[_]](annotation: F[Option[Annotation[F]]],
                                types: F[List[Type[F]]])
      extends AST[F]

  object Schema {
    def empty[F[_]: MonoidK](): Schema[F] =
      apply(MonoidK[F].empty, MonoidK[F].empty)
  }

}
