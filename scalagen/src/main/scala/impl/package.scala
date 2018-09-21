package sculptor.scalagen

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
}
