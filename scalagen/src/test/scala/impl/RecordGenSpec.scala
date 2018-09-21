package sculptor.scalagen
package impl

import cats.implicits._
import org.specs2._
import org.typelevel.paiges._

object RecordGenSpec extends mutable.Specification
    with ScalaCheck
    with testing.CatsEqMatcher {

  import testing.paiges._
  import sculptor.ast._
  import dsl._

  val cfg = Config()

  "RecordGen" should {

    "generate simple records" >> {

      val r = record("Record")
        .field("id", "Int".spec)
        .field("nameO", "Option".spec("String".spec))
        .build

      run(RecordGen.generate(r), cfg) must beEqvTo(
        Doc.text("final case class Record(id: Int, nameO: Option[String])").asRight
      )
    }

    "generate generic records" >> {

      val r = record("Record").generic("A".gen)
        .field("id", "Int".spec)
        .field("nameO", "Option".spec("A".gen))
        .build

      run(RecordGen.generate(r), cfg) must beEqvTo(
        Doc.text("final case class Record[A](id: Int, nameO: Option[A])").asRight
      )
    }

  }

}
