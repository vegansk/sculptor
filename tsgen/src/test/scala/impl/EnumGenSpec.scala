package sculptor.tsgen
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

    val e = enum("Colors")
      .comment("The Colors enum")
      .values(
        enumValue("Red").value("red").comment("Red color"),
        enumValue("Green").value("green").comment("Green color"),
        enumValue("Blue").value("blue").comment("Blue color")
      )
      .build

    "generate ADTs" >> {
      runGen(EnumGen.generate(e), cfg) must beEqvTo(
        """|export enum Colors {
           |  Red = "red",
           |  Green = "green",
           |  Blue = "blue"
           |}""".stripMargin.asRight
      )
    }

    "generate comments" >> {
      runGen(EnumGen.generate(e), cfg.copy(generateComments = true)) must beEqvTo(
        """|// Enum Colors: The Colors enum
           |
           |export enum Colors {
           |  Red = "red" /* Red color */,
           |  Green = "green" /* Green color */,
           |  Blue = "blue" /* Blue color */
           |}""".stripMargin.asRight
      )
    }
  }
}
