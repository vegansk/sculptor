package sculptor.scalagen
package impl

import org.specs2._
import cats.implicits._
import org.typelevel.paiges._

object AliasGenSpec extends mutable.Specification
    with ScalaCheck
    with testing.CatsEqMatcher {

  import testing.paiges._
  import sculptor.ast._
  import dsl._

  val cfg = Config()

  "AliasGen" should {

    "handle simple aliases" >> {

      val a = alias("MyInt").baseType(TypeRef.spec("Int")).build

      run(AliasGen.generate(a), cfg) must beEqvTo(
        Doc.text("type MyInt = Int").asRight
      )

    }

    "handle generic aliases" >> {

      val a = alias("Result").generic(TypeRef.gen("A")).baseType(TypeRef.spec("Either", TypeRef.spec("String"), TypeRef.gen("A"))).build

      run(AliasGen.generate(a), cfg) must beEqvTo(
        Doc.text("type Result[A] = Either[String, A]").asRight
      )
    }

    "handle upper bounds" >> {

      val a = alias("PetsList").generic(GenericDef.of("P", TypeRef.spec("Pet"), TypeRef.spec("FourLegged"))).baseType(TypeRef.spec("List", TypeRef.gen("P"))).build

      run(AliasGen.generate(a), cfg) must beEqvTo(
        Doc.text("type PetsList[P <: Pet with FourLegged] = List[P]").asRight
      )
    }

  }
}
