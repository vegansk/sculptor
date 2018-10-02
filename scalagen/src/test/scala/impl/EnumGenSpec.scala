package sculptor.scalagen
package impl

import cats.implicits._
import org.specs2._
import org.typelevel.paiges._

object EnumGenSpec extends mutable.Specification
    with ScalaCheck
    with testing.CatsEqMatcher {

  import testing.paiges._
  import sculptor.ast._
  import dsl._

  val cfg = Config()

  "EnumGenSpec" should {

    "generate ADTs" >> {

      val e = enum("Colors")
        .values(
          enumValue("Red").value("red"),
          enumValue("Green").value("green"),
          enumValue("Blue").value("blue")
        )
        .build

      run(EnumGen.generate(e), cfg) must beEqvTo(
        Doc.text("""|sealed trait Colors extends Product with Serializable
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
                    |}
                    |""".stripMargin).asRight
      )
    }

    "generate Eq typeclass" >> {
      val e = enum("Colors")
        .values(
          enumValue("Red").value("red")
        )
        .build

      run(EnumGen.generate(e), cfg.copy(features = List(Feature.CatsEqTypeclass))) must beEqvTo(
        Doc.text("""|sealed trait Colors extends Product with Serializable
                    |
                    |object Colors {
                    |  case object Red extends Colors
                    |
                    |  val asString: Colors => String = {
                    |    case Red => "red"
                    |  }
                    |
                    |  val fromString: PartialFunction[String, Colors] = {
                    |    case "red" => Red
                    |  }
                    |
                    |  implicit val ColorsEq: Eq[Colors] = Eq.fromUniversalEquals
                    |}
                    |""".stripMargin).asRight
      )
    }

    "generate circe codecs" >> {
      val e = enum("Colors")
        .values(
          enumValue("Red").value("red")
        )
        .build

      run(EnumGen.generate(e).map(_.render(cfg.lineWidth)), cfg.copy(features = List(Feature.CirceCodecs()))) must beEqvTo(
        s"""|sealed trait Colors extends Product with Serializable
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
  }
}
