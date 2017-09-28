package sculptor.xsd

import cats._

object ast {

  sealed trait Ast[F[_]]

  final case class Restriction[F[_]](base: F[String], simpleType: F[Option[SimpleType[F]]]) extends Ast[F]
  object Restriction {
    def apply[F[_]: MonoidK](): Restriction[F] =
      apply(
        MonoidK[F].empty,
        MonoidK[F].empty
      )
  }

  final case class SimpleType[F[_]](name: F[Option[String]], restriction: F[Option[Restriction[F]]]) extends Ast[F]
  object SimpleType {
    def apply[F[_]: MonoidK](): SimpleType[F] =
      apply(
        MonoidK[F].empty,
        MonoidK[F].empty
      )
  }

  final case class Module[F[_]](types: F[List[SimpleType[F]]]) extends Ast[F]
  object Module {
    def apply[F[_]: MonoidK](): Module[F] =
      apply(
        MonoidK[F].empty
      )
  }

}
