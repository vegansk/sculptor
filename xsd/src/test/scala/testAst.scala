package sculptor.xsd

import cats._

object testAst {

  sealed trait AST[F[_]]

  final case class Restriction[F[_]](base: F[String],
                                     simpleType: F[Option[SimpleType[F]]])
      extends AST[F]
  object Restriction {
    def apply[F[_]: MonoidK](): Restriction[F] =
      apply(MonoidK[F].empty, MonoidK[F].empty)
  }

  sealed trait Type[F[_]] extends AST[F]

  final case class SimpleType[F[_]](name: F[Option[String]],
                                    restriction: F[Option[Restriction[F]]])
      extends Type[F]
  object SimpleType {
    def apply[F[_]: MonoidK](): SimpleType[F] =
      apply(MonoidK[F].empty, MonoidK[F].empty)
  }

  final case class Module[F[_]](types: F[List[Type[F]]]) extends AST[F]
  object Module {
    def apply[F[_]: MonoidK](): Module[F] =
      apply(MonoidK[F].empty)
  }

}
