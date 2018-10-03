package sculptor.tsgen

import org.typelevel.paiges._
import cats.data._

package object impl {

  /**
    * Generator implementation result
    */
  type Result[A] = EitherT[State[GeneratorState, ?], String, A]

  def ok[A](v: A): Result[A] = EitherT.rightT(v)

  def error[A](err: String): Result[A] = EitherT.leftT(err)

  def getIndent: Result[Int] =
    EitherT.liftF(State.get[GeneratorState].map(s => Config.tabSize(s.config)))

  def getFeatures: Result[List[Feature]] =
    EitherT.liftF(State.get[GeneratorState].map(s => Config.features(s.config)))

  def getPrefixCode: Result[Option[Doc]] =
    EitherT.liftF(
      State.get[GeneratorState].map(s => Config.prefixCode(s.config))
    )

  def getAdtTag: Result[String] =
    EitherT.liftF(State.get[GeneratorState].map(s => Config.adtTag(s.config)))
}
