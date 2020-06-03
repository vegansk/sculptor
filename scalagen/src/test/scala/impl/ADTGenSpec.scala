package sculptor.scalagen
package impl

import cats.implicits._
import org.specs2._

object ADTGenSpec
    extends mutable.Specification
    with ScalaCheck
    with testing.Helpers {

  import sculptor.ast._
  import dsl._

  val cfg =
    Config(generateComments = false, generateAdtConstructorsHelpers = false)

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
      .additionalCodeS(
        "// Additional code comment",
        """def fold[A, B](empty: => B, f: A => B)(fa: Maybe[A]): B = ???""".fix
      )
      .build

    val maybeIntAdt = adt("MaybeInt")
      .constructors(cons("Empty"), cons("JustInt").field("value", "Int".spec))
      .build

    "generate ADTs" >> {

      "without constructors helpers" >> {
        runGen(ADTGen.generate(maybeAdt), cfg) must beEqvTo(
          """|sealed trait Maybe[A] extends Product with Serializable
             |
             |object Maybe {
             |  final case class Empty[A]() extends Maybe[A]
             |  final case class Just[A](value: A) extends Maybe[A]
             |}""".fix.asRight
        )
      }

      "with constructors helpers" >> {
        runGen(
          ADTGen.generate(maybeAdt),
          cfg.copy(generateAdtConstructorsHelpers = true)
        ) must beEqvTo(
          """|sealed trait Maybe[A] extends Product with Serializable
             |
             |object Maybe {
             |  final case class Empty[A]() extends Maybe[A]
             |  final case class Just[A](value: A) extends Maybe[A]
             |
             |  def empty[A]: Maybe[A] = Empty[A]()
             |  def just[A](value: A): Maybe[A] = Just[A](value)
             |}""".fix.asRight
        )
      }
    }

    "generate simple enums" >> {
      val a = adt("Colors")
        .constructors(cons("Red"), cons("Green"), cons("Blue"))
        .build

      "without constructors helpers" >> {
        runGen(ADTGen.generate(a), cfg) must beEqvTo(
          """|sealed trait Colors extends Product with Serializable
             |
             |object Colors {
             |  case object Red extends Colors
             |  case object Green extends Colors
             |  case object Blue extends Colors
             |}""".fix.asRight
        )
      }

      "with constructors helpers" >> {
        runGen(
          ADTGen.generate(a),
          cfg.copy(generateAdtConstructorsHelpers = true)
        ) must beEqvTo("""|sealed trait Colors extends Product with Serializable
             |
             |object Colors {
             |  case object Red extends Colors
             |  case object Green extends Colors
             |  case object Blue extends Colors
             |
             |  val red: Colors = Red
             |  val green: Colors = Green
             |  val blue: Colors = Blue
             |}""".fix.asRight)
      }
    }

    "generate Eq instance" >> {
      runGen(
        ADTGen.generate(maybeAdt),
        cfg.copy(features = List(Feature.CatsEqTypeclass))
      ) must beEqvTo(
        """|sealed trait Maybe[A] extends Product with Serializable
           |
           |object Maybe {
           |  final case class Empty[A]() extends Maybe[A]
           |  final case class Just[A](value: A) extends Maybe[A]
           |
           |  implicit def MaybeEq[A]: Eq[Maybe[A]] = Eq.fromUniversalEquals
           |}""".fix.asRight
      )
    }

    "generate Circe instances" >> {
      runGen(
        ADTGen.generate(maybeAdt),
        cfg.copy(features = List(Feature.CirceCodecs(adtTag = "__customTag")))
      ) must beEqvTo(
        """|sealed trait Maybe[A] extends Product with Serializable
           |
           |object Maybe {
           |  final case class Empty[A]() extends Maybe[A]
           |  final case class Just[A](value: A) extends Maybe[A]
           |
           |  implicit def MaybeEncoder[A:Encoder]: Encoder.AsObject[Maybe[A]] = Encoder.AsObject.instance[Maybe[A]] {
           |    case _: Empty[A] => JsonObject("__customTag" := "Empty")
           |    case v: Just[A] => JsonObject(
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
           |}""".fix.asRight
      )
    }

    "handle case objects in circe instances" >> {
      runGen(
        ADTGen.generate(maybeIntAdt),
        cfg.copy(features = List(Feature.CirceCodecs(adtTag = "__tag")))
      ) must beEqvTo(
        """|sealed trait MaybeInt extends Product with Serializable
           |
           |object MaybeInt {
           |  case object Empty extends MaybeInt
           |  final case class JustInt(value: Int) extends MaybeInt
           |
           |  implicit val MaybeIntEncoder: Encoder.AsObject[MaybeInt] = Encoder.AsObject.instance[MaybeInt] {
           |    case Empty => JsonObject("__tag" := "Empty")
           |    case v: JustInt => JsonObject(
           |      "__tag" := "JustInt",
           |      "value" := v.value
           |    )
           |  }
           |
           |  implicit val MaybeIntDecoder: Decoder[MaybeInt] = Decoder.instance[MaybeInt] { c =>
           |    c.downField("__tag").as[String].flatMap {
           |      case "Empty" => Right(Empty)
           |      case "JustInt" => for {value <- c.downField("value").as[Int]} yield JustInt(value)
           |      case tag => Left(DecodingFailure("Invalid ADT tag value MaybeInt." + tag, c.history))
           |    }
           |  }
           |}""".fix.asRight
      )
    }

    "generate comments" >> {
      runGen(ADTGen.generate(maybeAdt), cfg.copy(generateComments = true)) must beEqvTo(
        """|// ADT Maybe[A]:
           |// The type representing optional value
           |
           |sealed trait Maybe[A] extends Product with Serializable
           |
           |object Maybe {
           |  /* The empty value */
           |  final case class Empty[A]() extends Maybe[A]
           |  /* The non empty value */
           |  final case class Just[A](value: A /* The value */) extends Maybe[A]
           |}""".fix.asRight
      )
    }

    "generate additional code" >> {
      runGen(
        ADTGen.generate(maybeAdt),
        cfg.copy(features = Feature.AdditionalCode.pure[List])
      ) must beEqvTo(
        """|sealed trait Maybe[A] extends Product with Serializable
           |
           |object Maybe {
           |  final case class Empty[A]() extends Maybe[A]
           |  final case class Just[A](value: A) extends Maybe[A]
           |
           |  // Additional code comment
           |
           |  def fold[A, B](empty: => B, f: A => B)(fa: Maybe[A]): B = ???
           |}""".fix.asRight
      )
    }

  }

}
