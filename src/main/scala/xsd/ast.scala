package sculptor.xsd

import cats._

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
    enumeration: F[List[Enumeration[F]]]
  ) extends AST[F]

  object SimpleTypeRestriction {
    def empty[F[_]: MonoidK](): SimpleTypeRestriction[F] =
      apply(MonoidK[F].empty, MonoidK[F].empty, MonoidK[F].empty)
  }

  final case class SimpleType[F[_]](
    annotation: F[Option[Annotation[F]]],
    name: F[Option[String]],
    restriction: F[Option[SimpleTypeRestriction[F]]]
  ) extends AST[F]

  object SimpleType {
    def empty[F[_]: MonoidK](): SimpleType[F] =
      apply(MonoidK[F].empty, MonoidK[F].empty, MonoidK[F].empty)
  }

  final case class Type[F[_]](`type`: Type.AST[F]) extends AST[F]

  object Type {
    sealed trait AST[F[_]]

    final case class Simple[F[_]](`type`: SimpleType[F]) extends AST[F]
  }

  final case class Schema[F[_]](annotation: F[Option[Annotation[F]]],
                                types: F[List[Type[F]]])
      extends AST[F]

  object Schema {
    def empty[F[_]: MonoidK](): Schema[F] =
      apply(MonoidK[F].empty, MonoidK[F].empty)
  }

}
