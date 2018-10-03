package sculptor
package tsgen
package deprecated

import cats._
import cats.data._

import sculptor.xsd.{ast => x}
import ast._

package xsd {
  final case class ExternatType(xsdName: x.QName, name: QName, constName: QName)

  final case class Config(imports: List[ImportDecl],
                          xsdNs: Option[String],
                          externalTypes: List[ExternatType])
}

package object xsd {

  import Transform.TransformState

  type Result[A] = EitherT[Reader[Config, ?], String, A]
  type SrcF[A] = Id[A]

  def ok[A](v: A): Result[A] = EitherT.rightT(v)
  def error[A](err: String): Result[A] = EitherT.leftT(err)
  def getConfig: Result[Config] = EitherT.liftF(Kleisli.ask[Id, Config])

  def transform(xsd: x.Schema[Id]): Result[ModuleDecl] = {
    def transfState[A](s: State[TransformState, A]): State[Config, A] =
      s.transformS(c => TransformState(c, new Fold(c), Nil, Nil), (s, _) => s)
    def transfReader[A](s: State[Config, A]): Reader[Config, A] =
      Reader(c => s.run(c).value._2)
    // TODO: Use `mapK` after updating cats
    EitherT(transfReader(transfState(Transform.schema(xsd).value)))
  }
}
