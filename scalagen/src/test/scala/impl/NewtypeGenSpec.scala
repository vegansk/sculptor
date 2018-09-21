package sculptor.scalagen
package impl

import org.specs2._
import cats.implicits._
import org.typelevel.paiges._

object NewtypeGenSpec extends mutable.Specification
    with ScalaCheck
    with testing.CatsEqMatcher {

  import testing.paiges._
  import sculptor.ast._
  import dsl._

  val cfg = Config()

  "NewtypeGen" should {

    "handle simple aliases" >> {

      val n = newtype("MyInt")
        .baseType("Int".spec)
        .build

      run(NewtypeGen.generate(n), cfg) must beEqvTo(
        Doc.text("final case class MyInt(value: Int)").asRight
      )

    }

    "handle generic aliases" >> {

      val n = newtype("Result")
        .generic("A".gen)
        .baseType("Either".spec("String".spec, "A".gen))
        .build

      run(NewtypeGen.generate(n), cfg) must beEqvTo(
        Doc.text("final case class Result[A](value: Either[String, A])").asRight
      )
    }

    "handle upper bounds" >> {

      val n = newtype("PetsList")
        .generic("P".genExt("Pet".spec, "FourLegged".spec))
        .baseType("List".spec("P".gen))
        .build

      run(NewtypeGen.generate(n), cfg) must beEqvTo(
        Doc.text("final case class PetsList[P <: Pet with FourLegged](value: List[P])").asRight
      )
    }

  }
}
