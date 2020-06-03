package sculptor.tsgen
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
           |}
           |
           |export const colorsValues: Colors[] = [Colors.Red, Colors.Green, Colors.Blue]""".fix.asRight
      )
    }

    "generate full descriptions" >> {
      runGen(EnumGen.generate(e), cfg.copy(generateEnumsDescriptions = true)) must beEqvTo(
        """|export enum Colors {
           |  Red = "red",
           |  Green = "green",
           |  Blue = "blue"
           |}
           |
           |export const colorsValues: Colors[] = [Colors.Red, Colors.Green, Colors.Blue]
           |
           |export const colorsDescription = (v: Colors): string => {
           |  switch(v) {
           |    case Colors.Red: return "Red color"
           |    case Colors.Green: return "Green color"
           |    case Colors.Blue: return "Blue color"
           |  }
           |}""".fix.asRight
      )
    }

    "generate partial descriptions" >> {
      val e0 = e.copy(
        values = NonEmptyList(e.values.head.copy(comment = None), e.values.tail)
      )
      runGen(EnumGen.generate(e0), cfg.copy(generateEnumsDescriptions = true)) must beEqvTo(
        """|export enum Colors {
           |  Red = "red",
           |  Green = "green",
           |  Blue = "blue"
           |}
           |
           |export const colorsValues: Colors[] = [Colors.Red, Colors.Green, Colors.Blue]
           |
           |export const colorsDescription = (v: Colors): string | undefined => {
           |  switch(v) {
           |    case Colors.Green: return "Green color"
           |    case Colors.Blue: return "Blue color"
           |  }
           |}""".fix.asRight
      )
    }

    "generate comments" >> {
      runGen(EnumGen.generate(e), cfg.copy(generateComments = true)) must beEqvTo(
        """|// Enum Colors:
           |// The Colors enum
           |
           |export enum Colors {
           |  Red = "red" /* Red color */,
           |  Green = "green" /* Green color */,
           |  Blue = "blue" /* Blue color */
           |}
           |
           |export const colorsValues: Colors[] = [Colors.Red, Colors.Green, Colors.Blue]""".fix.asRight
      )
    }

    "generate multiline type comments" >> {
      val e = enum("Foo")
        .comment(
          """|Lorem
             |ipsum
             |dolor"""
            .stripMargin
        ).values(
          enumValue("Bar")
        ).build

      runGen(EnumGen.generate(e), cfg.copy(generateComments = true)) must beEqvTo(
        """|// Enum Foo:
           |// Lorem
           |// ipsum
           |// dolor
           |
           |export enum Foo {Bar = "Bar"}
           |
           |export const fooValues: Foo[] = [Foo.Bar]""".fix.asRight
      )
    }
  }
}
