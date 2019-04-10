package sculptor.scalagen

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

  def getGenerateComments: Result[Boolean] =
    EitherT.liftF(
      State.get[GeneratorState].map(s => Config.generateComments(s.config))
    )

  def getGenerateAdtConstructorsHelpers: Result[Boolean] =
    EitherT.liftF(
      State
        .get[GeneratorState]
        .map(s => Config.generateAdtConstructorsHelpers(s.config))
    )
}
