package sculptor.tsgen
package impl

import cats.implicits._
import org.specs2._

object RecordGenSpec extends mutable.Specification
    with ScalaCheck
    with testing.CatsEqMatcher {

  import sculptor.ast._
  import dsl._

  val cfg = Config()

  "RecordGen" should {

    "generate simple records" >> {

      val r = record("Record")
        .field("id", "number".spec)
        .field("nameO", "Option".spec("string".spec))
        .build

      run(RecordGen.generate(r).map(_.render(cfg.lineWidth)), cfg) must beEqvTo(
        """|export interface Record {
           |  id: number
           |  nameO: Option<string>
           |}""".stripMargin.asRight
      )
    }

    "generate generic records" >> {

      val r = record("Record").generic("A".gen)
        .field("id", "number".spec)
        .field("nameO", "Option".spec("A".gen))
        .build

      run(RecordGen.generate(r).map(_.render(cfg.lineWidth)), cfg) must beEqvTo(
        """|export interface Record<A> {
           |  id: number
           |  nameO: Option<A>
           |}""".stripMargin.asRight
      )
    }
  }

}
