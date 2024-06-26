package sculptor.scalagen
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
      .baseType(TypeRef.spec("Int"))
      .comment("The Int type alias")
      .build

    "handle simple aliases" >> {

      runGen(AliasGen.generate(myInt), cfg) must beEqvTo(
        "type MyInt = Int".fix.asRight
      )

    }

    "handle generic aliases" >> {

      val a = alias("Result")
        .generic(TypeRef.gen("A"))
        .baseType(
          TypeRef.spec("Either", TypeRef.spec("String"), TypeRef.gen("A"))
        )
        .build

      runGen(AliasGen.generate(a), cfg) must beEqvTo(
        "type Result[A] = Either[String, A]".fix.asRight
      )
    }

    "handle upper bounds" >> {

      val a = alias("PetsList")
        .generic(
          GenericDef.of("P", TypeRef.spec("Pet"), TypeRef.spec("FourLegged"))
        )
        .baseType(TypeRef.spec("List", TypeRef.gen("P")))
        .build

      runGen(AliasGen.generate(a), cfg) must beEqvTo(
        "type PetsList[P <: Pet with FourLegged] = List[P]".fix.asRight
      )
    }

    "generate comments" >> {
      runGen(AliasGen.generate(myInt), cfg.copy(generateComments = true)) must beEqvTo(
        """|// Alias MyInt:
           |
           |/** The Int type alias */
           |type MyInt = Int""".fix.asRight
      )
    }

    "escape reserved words" >> {
      val a = alias("case")
        .generic(GenericDef.of("P", TypeRef.spec("val"), TypeRef.spec("class")))
        .baseType(TypeRef.spec("object", TypeRef.gen("P")))
        .build

      runGen(AliasGen.generate(a), cfg) must beEqvTo(
        "type `case`[P <: `val` with `class`] = `object`[P]".fix.asRight
      )
    }

  }
}
