package sculptor.tsgen
package impl

import cats.implicits._
import org.specs2._

object RecordGenSpec extends mutable.Specification
    with ScalaCheck
    with testing.Helpers {

  import sculptor.ast._
  import dsl._

  val cfg = Config()

  "RecordGen" should {

    val r = record("Record").generic("A".gen)
      .field("id", "number".spec)
      .field("nameO", "Option".spec("A".gen))
      .build

    "generate records" >> {
      run(RecordGen.generate(r).map(_.render(cfg.lineWidth)), cfg) must beEqvTo(
        """|export interface Record<A> {
           |  id: number
           |  nameO: Option<A>
           |}""".stripMargin.asRight
      )
    }

    "handle optional encoding" >> {
      runGen(RecordGen.generate(r), cfg.copy(optionalEncoding = OptionalEncoding("Option"))) must beEqvTo(
        """|export interface Record<A> {
           |  id: number
           |  nameO?: A
           |}""".stripMargin.asRight
      )
    }

    "handle all fields optional encoding" >> {
      runGen(RecordGen.generate(r), cfg.copy(optionalEncoding = OptionalEncoding(allFieldsOptional = true))) must beEqvTo(
        """|export interface Record<A> {
           |  id?: number
           |  nameO?: Option<A>
           |}""".stripMargin.asRight
      )
    }

    "handle all fields optional encoding with optional class" >> {
      runGen(RecordGen.generate(r), cfg.copy(optionalEncoding = OptionalEncoding("Option", true))) must beEqvTo(
        """|export interface Record<A> {
           |  id?: number
           |  nameO?: A
           |}""".stripMargin.asRight
      )
    }
  }

}
