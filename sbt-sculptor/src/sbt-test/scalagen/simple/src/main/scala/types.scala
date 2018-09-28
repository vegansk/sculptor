package test.types

final case class MyInt(value: Int) extends AnyVal

final case class Record(id: Int, name: String)

sealed trait Maybe[A] extends Product with Serializable

object Maybe {
  final case class Nothing[A]() extends Maybe[A]
  final case class Just[A](get: A) extends Maybe[A]
}

sealed trait Enum extends Product with Serializable

object Enum {
  case object A extends Enum
  case object B extends Enum
  case object C extends Enum
  
  val asString: Enum => String = {
    case A => "A"
    case B => "B"
    case C => "C"
  }
  
  val fromString: PartialFunction[String, Enum] = {
    case "A" => A
    case "B" => B
    case "C" => C
  }
}