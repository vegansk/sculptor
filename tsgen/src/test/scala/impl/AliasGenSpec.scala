package sculptor.tsgen
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

      val a = alias("MyInt").baseType(TypeRef.spec("number")).build

      run(AliasGen.generate(a).map(_.render(cfg.lineWidth)), cfg) must beEqvTo(
        "type MyInt = number".asRight
      )

    }

    "handle generic aliases" >> {

      val a = alias("Result").generic(TypeRef.gen("A")).baseType(TypeRef.spec("Either", TypeRef.spec("string"), TypeRef.gen("A"))).build

      run(AliasGen.generate(a).map(_.render(cfg.lineWidth)), cfg) must beEqvTo(
        "type Result<A> = Either<string, A>".asRight
      )
    }

    "handle upper bounds" >> {

      val a = alias("PetsList").generic(GenericDef.of("P", TypeRef.spec("Pet"), TypeRef.spec("FourLegged"))).baseType(TypeRef.spec("Array", TypeRef.gen("P"))).build

      run(AliasGen.generate(a), cfg) must beEqvTo(
        Doc.text("type PetsList<P extends Pet & FourLegged> = Array<P>").asRight
      )
    }

  }
}
