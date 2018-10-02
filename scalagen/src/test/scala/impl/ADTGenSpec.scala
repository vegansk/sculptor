package sculptor.scalagen
package impl

import cats.implicits._
import org.specs2._
import org.typelevel.paiges._

object ADTGenSpec extends mutable.Specification
    with ScalaCheck
    with testing.CatsEqMatcher {

  import testing.paiges._
  import sculptor.ast._
  import dsl._

  val cfg = Config()

  "ADTGen" should {

    "generate ADTs" >> {

      val a = adt("Maybe")
        .generic("A".gen)
        .constructors(
          cons("Empty").generic("A".gen),
          cons("Just")
            .generic("A".gen)
            .field("value", "A".gen)
        )
        .build

      run(ADTGen.generate(a), cfg) must beEqvTo(
        Doc.text("""|sealed trait Maybe[A] extends Product with Serializable
                    |
                    |object Maybe {
                    |  final case class Empty[A]() extends Maybe[A]
                    |  final case class Just[A](value: A) extends Maybe[A]
                    |}
                    |""".stripMargin).asRight
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

      run(ADTGen.generate(a), cfg) must beEqvTo(
        Doc.text("""|sealed trait Colors extends Product with Serializable
                    |
                    |object Colors {
                    |  case object Red extends Colors
                    |  case object Green extends Colors
                    |  case object Blue extends Colors
                    |}
                    |""".stripMargin).asRight
      )
    }

    val maybeAdt = adt("Maybe")
      .generic("A".gen)
      .constructors(
        cons("Empty").generic("A".gen),
        cons("Just")
          .generic("A".gen)
          .field("value", "A".gen)
      )
      .build

    "generate Eq instance" >> {
      run(ADTGen.generate(maybeAdt), cfg.copy(features = List(Feature.CatsEqTypeclass))) must beEqvTo(
        Doc.text("""|sealed trait Maybe[A] extends Product with Serializable
                    |
                    |object Maybe {
                    |  final case class Empty[A]() extends Maybe[A]
                    |  final case class Just[A](value: A) extends Maybe[A]
                    |
                    |  implicit def MaybeEq[A]: Eq[Maybe[A]] = Eq.fromUniversalEquals
                    |}
                    |""".stripMargin).asRight
      )
    }

    "generate Circe instances" >> {
      run(ADTGen.generate(maybeAdt), cfg.copy(features = List(Feature.CirceCodecs(adtTag = "__customTag")))) must beEqvTo(
        Doc.text("""|sealed trait Maybe[A] extends Product with Serializable
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
                    |      case "Just" => for {
                    |        value <- c.downField("value").as[A]
                    |      } yield Just[A](value)
                    |      case tag => Left(DecodingFailure("Invalid ADT tag value Maybe." + tag, c.history))
                    |    }
                    |  }
                    |}
                    |""".stripMargin).asRight
      )
    }

  }

}
