package sculptor.tsgen
package impl

import org.specs2._
import cats.implicits._

object AliasGenSpec
    extends mutable.Specification
    with ScalaCheck
    with testing.Helpers {

  import sculptor.ast._
  import dsl._

  val cfg = Config(generateComments = false)

  "AliasGen" should {

    val myInt = alias("MyInt")
      .baseType(TypeRef.spec("number"))
      .comment("The Int type alias")
      .build

    "handle simple aliases" >> {
      runGen(AliasGen.generate(myInt), cfg) must beEqvTo(
        "export type MyInt = number".fix.asRight
      )
    }

    "handle generic aliases" >> {
      val a = alias("Result")
        .generic(TypeRef.gen("A"))
        .baseType(
          TypeRef.spec("Either", TypeRef.spec("string"), TypeRef.gen("A"))
        )
        .build

      runGen(AliasGen.generate(a), cfg) must beEqvTo(
        "export type Result<A> = Either<string, A>".fix.asRight
      )
    }

    "handle upper bounds" >> {
      val a = alias("PetsList")
        .generic(
          GenericDef.of("P", TypeRef.spec("Pet"), TypeRef.spec("FourLegged"))
        )
        .baseType(TypeRef.spec("Array", TypeRef.gen("P")))
        .build

      runGen(AliasGen.generate(a), cfg) must beEqvTo(
        "export type PetsList<P extends Pet & FourLegged> = Array<P>".fix.asRight
      )
    }

    "generate comments" >> {
      runGen(AliasGen.generate(myInt), cfg.copy(generateComments = true)) must beEqvTo(
        """|// Alias MyInt:
           |
           |/** The Int type alias */
           |export type MyInt = number""".fix.asRight
      )
    }

  }
}
