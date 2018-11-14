package sculptor.scalagen
package impl

import cats.implicits._
import org.specs2._

object ADTGenSpec extends mutable.Specification
    with ScalaCheck
    with testing.Helpers {

  import sculptor.ast._
  import dsl._

  val cfg = Config(generateComments = false)

  "ADTGen" should {

    val maybeAdt = adt("Maybe")
      .generic("A".gen)
      .constructors(
        cons("Empty").generic("A".gen).comment("The empty value"),
        cons("Just")
          .generic("A".gen)
          .field("value", "A".gen, comment = "The value")
          .comment("The non empty value")
      )
      .comment("The type representing optional value")
      .build

    "generate ADTs" >> {

      runGen(ADTGen.generate(maybeAdt), cfg) must beEqvTo(
        """|sealed trait Maybe[A] extends Product with Serializable
           |
           |object Maybe {
           |  final case class Empty[A]() extends Maybe[A]
           |  final case class Just[A](value: A) extends Maybe[A]
           |}""".stripMargin.asRight
      )
    }

    "generate simple enums" >> {
      val a = adt("Colors")
        .constructors(
          cons("Red"),
          cons("Green"),
          cons("Blue")
        )
        .build

      runGen(ADTGen.generate(a), cfg) must beEqvTo(
        """|sealed trait Colors extends Product with Serializable
           |
           |object Colors {
           |  case object Red extends Colors
           |  case object Green extends Colors
           |  case object Blue extends Colors
           |}""".stripMargin.asRight
      )
    }

    "generate Eq instance" >> {
      runGen(ADTGen.generate(maybeAdt), cfg.copy(features = List(Feature.CatsEqTypeclass))) must beEqvTo(
        """|sealed trait Maybe[A] extends Product with Serializable
           |
           |object Maybe {
           |  final case class Empty[A]() extends Maybe[A]
           |  final case class Just[A](value: A) extends Maybe[A]
           |
           |  implicit def MaybeEq[A]: Eq[Maybe[A]] = Eq.fromUniversalEquals
           |}""".stripMargin.asRight
      )
    }

    "generate Circe instances" >> {
      runGen(ADTGen.generate(maybeAdt), cfg.copy(features = List(Feature.CirceCodecs(adtTag = "__customTag")))) must beEqvTo(
        """|sealed trait Maybe[A] extends Product with Serializable
           |
           |object Maybe {
           |  final case class Empty[A]() extends Maybe[A]
           |  final case class Just[A](value: A) extends Maybe[A]
           |
           |  implicit def MaybeEncoder[A:Encoder]: ObjectEncoder[Maybe[A]] = ObjectEncoder.instance[Maybe[A]] {
           |    case _:Empty[A] => JsonObject("__customTag" := "Empty")
           |    case v:Just[A] => JsonObject(
           |      "__customTag" := "Just",
           |      "value" := v.value
           |    )
           |  }
           |
           |  implicit def MaybeDecoder[A:Decoder]: Decoder[Maybe[A]] = Decoder.instance[Maybe[A]] { c =>
           |    c.downField("__customTag").as[String].flatMap {
           |      case "Empty" => Right(Empty[A]())
           |      case "Just" => for {value <- c.downField("value").as[A]} yield Just[A](value)
           |      case tag => Left(DecodingFailure("Invalid ADT tag value Maybe." + tag, c.history))
           |    }
           |  }
           |}""".stripMargin.asRight
      )
    }

    "generate comments" >> {
      runGen(ADTGen.generate(maybeAdt), cfg.copy(generateComments = true)) must beEqvTo(
        """|// ADT Maybe[A]: The type representing optional value
           |
           |sealed trait Maybe[A] extends Product with Serializable
           |
           |object Maybe {
           |  // The empty value
           |  final case class Empty[A]() extends Maybe[A]
           |  // The non empty value
           |  final case class Just[A](value: A /* The value */) extends Maybe[A]
           |}""".stripMargin.asRight
      )
    }

  }

}
