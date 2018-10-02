package test.types

import cats._
import io.circe._
import io.circe.syntax._


final case class MyInt(value: Int) extends AnyVal

object MyInt {
  implicit val MyIntEq: Eq[MyInt] = Eq.fromUniversalEquals

  implicit val MyIntEncoder: Encoder[MyInt] = Encoder[Int].contramap(_.value)

  implicit val MyIntDecoder: Decoder[MyInt] = Decoder[Int].map(MyInt(_))
}

final case class Record(id: Int, name: String)

object Record {
  implicit val RecordEq: Eq[Record] = Eq.fromUniversalEquals

  implicit val RecordEncoder: ObjectEncoder[Record] = ObjectEncoder.instance[Record] { v =>
    JsonObject(
      "id" := v.id,
      "name" := v.name
    )
  }

  implicit val RecordDecoder: Decoder[Record] = Decoder.instance[Record] { c =>
    for {
      id <- c.downField("id").as[Int]
      name <- c.downField("name").as[String]
    } yield Record(id, name)
  }
}

sealed trait Maybe[A] extends Product with Serializable

object Maybe {
  final case class Nothing[A]() extends Maybe[A]
  final case class Just[A](get: A) extends Maybe[A]

  implicit def MaybeEq[A]: Eq[Maybe[A]] = Eq.fromUniversalEquals

  implicit def MaybeEncoder[A:Encoder]: ObjectEncoder[Maybe[A]] = ObjectEncoder.instance[Maybe[A]] {
    case _:Nothing[A] => JsonObject("__tag" := "Nothing")
    case v:Just[A] => JsonObject(
      "__tag" := "Just",
      "get" := v.get
    )
  }

  implicit def MaybeDecoder[A:Decoder]: Decoder[Maybe[A]] = Decoder.instance[Maybe[A]] { c =>
    c.downField("__tag").as[String].flatMap {
      case "Nothing" => Right(Nothing[A]())
      case "Just" => for {get <- c.downField("get").as[A]} yield Just[A](get)
      case tag => Left(DecodingFailure("Invalid ADT tag value Maybe." + tag, c.history))
    }
  }
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

  implicit val EnumEq: Eq[Enum] = Eq.fromUniversalEquals

  implicit val EnumEncoder: Encoder[Enum] = Encoder[String].contramap(Enum.asString(_))

  implicit val EnumDecoder: Decoder[Enum] = Decoder[String].emap(v => Enum.fromString.lift(v).toRight("Invalid enum value Enum." + v))
}