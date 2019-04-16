package sculptor.tsgen
package impl

import org.specs2._
import cats.implicits._

object NewtypeGenSpec
    extends mutable.Specification
    with ScalaCheck
    with testing.Helpers {

  import sculptor.ast._
  import dsl._

  val cfg = Config(generateComments = false)

  "NewtypeGen" should {

    val myInt = newtype("MyInt")
      .baseType("number".spec)
      .comment("The Int")
      .build

    "handle simple aliases" >> {
      runGen(NewtypeGen.generate(myInt), cfg) must beEqvTo(
        """export type MyInt = number & {__brand: "MyInt"}""".asRight
      )
    }

    "handle generic aliases" >> {

      val n = newtype("Result")
        .generic("A".gen)
        .baseType("Either".spec("string".spec, "A".gen))
        .build

      runGen(NewtypeGen.generate(n), cfg) must beEqvTo(
        """export type Result<A> = Either<string, A> & {__brand: "Result<A>"}""".asRight
      )
    }

    "handle upper bounds" >> {

      val n = newtype("PetsList")
        .generic("P".genExt("Pet".spec, "FourLegged".spec))
        .baseType("Array".spec("P".gen))
        .build

      runGen(NewtypeGen.generate(n), cfg) must beEqvTo(
        """export type PetsList<P extends Pet & FourLegged> = Array<P> & {__brand: "PetsList<P extends Pet & FourLegged>"}""".asRight
      )
    }

    "generate comments" >> {
      runGen(NewtypeGen.generate(myInt), cfg.copy(generateComments = true)) must beEqvTo(
        """|// Newtype MyInt: The Int
           |
           |export type MyInt = number & {__brand: "MyInt"}""".stripMargin.asRight
      )
    }
  }
}
