package sculptor.scalagen
package impl

import cats.data.NonEmptyList
import cats.implicits._
import org.specs2._

object EnumGenSpec
    extends mutable.Specification
    with ScalaCheck
    with testing.Helpers {

  import sculptor.ast._
  import dsl._

  val cfg = Config(generateComments = false)

  "EnumGenSpec" should {

    val testEnum = enum("Colors")
      .values(
        enumValue("Red").value("red").comment("Red color"),
        enumValue("Green").value("green").comment("Green color"),
        enumValue("Blue").value("blue").comment("Blue color")
      )
      .comment("The Colors enum")
      .additionalCodeS("// Additional comment")
      .build

    "generate ADTs" >> {

      runGen(EnumGen.generate(testEnum), cfg) must beEqvTo(
        """|sealed trait Colors extends Product with Serializable
           |
           |object Colors {
           |  case object Red extends Colors
           |  case object Green extends Colors
           |  case object Blue extends Colors
           |
           |  val values: List[Colors] = List(Red, Green, Blue)
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
           |}""".fix.asRight
      )
    }

    "generate Eq typeclass" >> {
      runGen(
        EnumGen.generate(testEnum),
        cfg.copy(features = List(Feature.CatsEqTypeclass))
      ) must beEqvTo(
        """|sealed trait Colors extends Product with Serializable
           |
           |object Colors {
           |  case object Red extends Colors
           |  case object Green extends Colors
           |  case object Blue extends Colors
           |
           |  val values: List[Colors] = List(Red, Green, Blue)
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
           |
           |  implicit val ColorsEq: Eq[Colors] = Eq.fromUniversalEquals
           |}""".fix.asRight
      )
    }

    "generate circe codecs" >> {
      runGen(
        EnumGen.generate(testEnum),
        cfg.copy(features = List(Feature.CirceCodecs()))
      ) must beEqvTo(
        """|sealed trait Colors extends Product with Serializable
           |
           |object Colors {
           |  case object Red extends Colors
           |  case object Green extends Colors
           |  case object Blue extends Colors
           |
           |  val values: List[Colors] = List(Red, Green, Blue)
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
           |
           |  implicit val ColorsEncoder: Encoder[Colors] = Encoder[String].contramap(Colors.asString(_))
           |
           |  implicit val ColorsDecoder: Decoder[Colors] = Decoder[String].emap(v => Colors.fromString.lift(v).toRight("Invalid enum value Colors." + v))
           |}""".fix.asRight
      )
    }

    "generate comments" >> {
      runGen(EnumGen.generate(testEnum), cfg.copy(generateComments = true)) must beEqvTo(
        """|// Enum Colors:
           |// The Colors enum
           |
           |sealed trait Colors extends Product with Serializable
           |
           |object Colors {
           |  // Red color
           |  case object Red extends Colors
           |  // Green color
           |  case object Green extends Colors
           |  // Blue color
           |  case object Blue extends Colors
           |
           |  val values: List[Colors] = List(Red, Green, Blue)
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
           |}""".fix.asRight
      )
    }

    "generate full descriptions" >> {
      runGen(
        EnumGen.generate(testEnum),
        cfg.copy(generateEnumsDescriptions = true)
      ) must beEqvTo("""|sealed trait Colors extends Product with Serializable
           |
           |object Colors {
           |  case object Red extends Colors
           |  case object Green extends Colors
           |  case object Blue extends Colors
           |
           |  val values: List[Colors] = List(Red, Green, Blue)
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
           |
           |  val description: Colors => String = {
           |    case Red => "Red color"
           |    case Green => "Green color"
           |    case Blue => "Blue color"
           |  }
           |}""".fix.asRight)
    }

    "generate partial descriptions" >> {
      val e = testEnum.copy(
        values = NonEmptyList(
          testEnum.values.head.copy(comment = None),
          testEnum.values.tail
        )
      )
      runGen(EnumGen.generate(e), cfg.copy(generateEnumsDescriptions = true)) must beEqvTo(
        """|sealed trait Colors extends Product with Serializable
           |
           |object Colors {
           |  case object Red extends Colors
           |  case object Green extends Colors
           |  case object Blue extends Colors
           |
           |  val values: List[Colors] = List(Red, Green, Blue)
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
           |
           |  val description: PartialFunction[Colors, String] = {
           |    case Green => "Green color"
           |    case Blue => "Blue color"
           |  }
           |}""".fix.asRight
      )
    }

    "generate additional code" >> {
      runGen(
        EnumGen.generate(testEnum),
        cfg.copy(features = List(Feature.AdditionalCode))
      ) must beEqvTo("""|sealed trait Colors extends Product with Serializable
           |
           |object Colors {
           |  case object Red extends Colors
           |  case object Green extends Colors
           |  case object Blue extends Colors
           |
           |  val values: List[Colors] = List(Red, Green, Blue)
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
           |
           |  // Additional comment
           |}""".fix.asRight)
    }

    "generate tapir Schema and Validator" >> {
      runGen(
        EnumGen.generate(testEnum),
        cfg.copy(features = List(Feature.TapirSchema()))
      ) must beEqvTo("""|sealed trait Colors extends Product with Serializable
           |
           |object Colors {
           |  case object Red extends Colors
           |  case object Green extends Colors
           |  case object Blue extends Colors
           |
           |  val values: List[Colors] = List(Red, Green, Blue)
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
           |
           |  implicit val ColorsSchema: Schema[Colors] =
           |    Schema(SchemaType.SString)
           |      .description("The Colors enum")
           |
           |  implicit val ColorsValidator: Validator[Colors] =
           |    Validator.enum(values, x => Some(asString(x)))
           |}""".fix.asRight)
    }
  }
}
