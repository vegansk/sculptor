package sculptor.tsgen
package impl

import cats.implicits._
import org.specs2._

object RecordGenSpec
    extends mutable.Specification
    with ScalaCheck
    with testing.Helpers {

  import sculptor.ast._
  import dsl._

  val cfg = Config(generateComments = false)

  "RecordGen" should {

    val r = record("Record")
      .generic("A".gen)
      .comment("The Record")
      .field("id", "number".spec, "The id")
      .field("nameO", "Option".spec("A".gen), "The name")
      .build

    "generates records" >> {
      runGen(RecordGen.generate(r), cfg) must beEqvTo(
        """|export interface Record<A> {
           |  id: number
           |  nameO: Option<A>
           |}""".fix.asRight
      )
    }

    "handles optional encoding" >> {
      runGen(
        RecordGen.generate(r),
        cfg.copy(optionalEncoding = OptionalEncoding("Option"))
      ) must beEqvTo("""|export interface Record<A> {
           |  id: number
           |  nameO?: A
           |}""".fix.asRight)
    }

    "handles all fields optional encoding" >> {
      runGen(
        RecordGen.generate(r),
        cfg.copy(optionalEncoding = OptionalEncoding(allFieldsOptional = true))
      ) must beEqvTo("""|export interface Record<A> {
           |  id?: number
           |  nameO?: Option<A>
           |}""".fix.asRight)
    }

    "handles all fields optional encoding with optional class" >> {
      runGen(
        RecordGen.generate(r),
        cfg.copy(optionalEncoding = OptionalEncoding("Option", true))
      ) must beEqvTo("""|export interface Record<A> {
           |  id?: number
           |  nameO?: A
           |}""".fix.asRight)
    }

    "generates comments" >> {
      runGen(RecordGen.generate(r), cfg.copy(generateComments = true)) must beEqvTo(
        """|// Record Record<A>:
           |// The Record
           |
           |export interface Record<A> {
           |  id: number /* The id */
           |  nameO: Option<A> /* The name */
           |}""".fix.asRight
      )
    }

    "generates bounds" >> {
      val r = record("Test")
        .generic("A".genExt("string".spec))
        .field("v", "A".gen)
        .build
      runGen(RecordGen.generate(r), cfg) must beEqvTo(
        """export interface Test<A extends string> {v: A}""".fix.asRight
      )
    }

  }

}
