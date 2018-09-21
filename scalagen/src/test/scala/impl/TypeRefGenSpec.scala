package sculptor.scalagen
package impl

import org.specs2._
import cats.implicits._
import org.typelevel.paiges._

object TypeRefGenSpec extends mutable.Specification
    with ScalaCheck
    with testing.CatsEqMatcher {

  import testing.paiges._
  import sculptor.ast._
  import dsl._

  val cfg = Config()

  "TypeRefGen" should {

    "handle generic types" >> {

      val g = "A".gen

      run(TypeRefGen.generate(g), cfg) must beEqvTo(
        Doc.text("A").asRight
      )

    }

    "handle specialized types" >> {
      val s = "Int".spec

      run(TypeRefGen.generate(s), cfg) must beEqvTo(
        Doc.text("Int").asRight
      )
    }

    "handle specialized types with parameters" >> {

      val s = "Either".spec("String".spec, "A".gen)

      run(TypeRefGen.generate(s), cfg) must beEqvTo(
        Doc.text("Either[String, A]").asRight
      )

    }

  }

}
