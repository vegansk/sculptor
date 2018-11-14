package sculptor.scalagen
package impl

import cats.implicits._
import org.specs2._

object EnumGenSpec extends mutable.Specification
    with ScalaCheck
    with testing.Helpers {

  import sculptor.ast._
  import dsl._

  val cfg = Config(generateComments = false)

  "EnumGenSpec" should {

    "generate ADTs" >> {

      val e = enum("Colors")
        .values(
          enumValue("Red").value("red"),
          enumValue("Green").value("green"),
          enumValue("Blue").value("blue")
        )
        .build

      runGen(EnumGen.generate(e), cfg) must beEqvTo(
        """|sealed trait Colors extends Product with Serializable
           |
           |object Colors {
           |  case object Red extends Colors
           |  case object Green extends Colors
           |  case object Blue extends Colors
           |
           |  val asString: Colors => String = {
           |    case Red => "red"
           |    case Green => "green"
           |    case Blue => "blue"
           |  }
           |
           |  val fromString: PartialFunction[String, Colors] = {
           |    case "red" => Red
           |    case "green" => Green
           |    case "blue" => Blue
           |  }
           |}""".stripMargin.asRight
      )
    }

    val testEnum = enum("Colors")
      .values(
        enumValue("Red").value("red").comment("Red color")
      )
      .comment("The Colors enum")
      .build

    "generate Eq typeclass" >> {
      runGen(EnumGen.generate(testEnum), cfg.copy(features = List(Feature.CatsEqTypeclass))) must beEqvTo(
        """|sealed trait Colors extends Product with Serializable
           |
           |object Colors {
           |  case object Red extends Colors
           |
           |  val asString: Colors => String = {case Red => "red"}
           |
           |  val fromString: PartialFunction[String, Colors] = {case "red" => Red}
           |
           |  implicit val ColorsEq: Eq[Colors] = Eq.fromUniversalEquals
           |}""".stripMargin.asRight
      )
    }

    "generate circe codecs" >> {
      runGen(EnumGen.generate(testEnum), cfg.copy(features = List(Feature.CirceCodecs()))) must beEqvTo(
        """|sealed trait Colors extends Product with Serializable
           |
           |object Colors {
           |  case object Red extends Colors
           |
           |  val asString: Colors => String = {case Red => "red"}
           |
           |  val fromString: PartialFunction[String, Colors] = {case "red" => Red}
           |
           |  implicit val ColorsEncoder: Encoder[Colors] = Encoder[String].contramap(Colors.asString(_))
           |
           |  implicit val ColorsDecoder: Decoder[Colors] = Decoder[String].emap(v => Colors.fromString.lift(v).toRight("Invalid enum value Colors." + v))
           |}""".stripMargin.asRight
      )
    }

    "generate comments" >> {
      runGen(EnumGen.generate(testEnum), cfg.copy(generateComments = true)) must beEqvTo(
        """|// Enum Colors: The Colors enum
           |
           |sealed trait Colors extends Product with Serializable
           |
           |object Colors {
           |  // Red color
           |  case object Red extends Colors
           |
           |  val asString: Colors => String = {case Red => "red"}
           |
           |  val fromString: PartialFunction[String, Colors] = {case "red" => Red}
           |}""".stripMargin.asRight
      )
    }
  }
}
