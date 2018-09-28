package test.types

final case class MyInt(value: Int) extends AnyVal
final case class Record(id: Int, name: String)
sealed trait Maybe[A] extends Product with Serializable

object Maybe {final case class Nothing[A]() extends Maybe[A]; final case class Just[A](get: A) extends Maybe[A]}