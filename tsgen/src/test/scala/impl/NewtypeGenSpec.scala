package sculptor.tsgen
package impl

import org.specs2._
import cats.implicits._

object NewtypeGenSpec extends mutable.Specification
    with ScalaCheck
    with testing.CatsEqMatcher {

  import sculptor.ast._
  import dsl._

  val cfg = Config()

  "NewtypeGen" should {

    "handle simple aliases" >> {

      val n = newtype("MyInt")
        .baseType("number".spec)
        .build

      run(NewtypeGen.generate(n).map(_.render(cfg.lineWidth)), cfg) must beEqvTo(
        """export type MyInt = number & {__brand: "MyInt"}""".asRight
      )

    }

    "handle generic aliases" >> {

      val n = newtype("Result")
        .generic("A".gen)
        .baseType("Either".spec("string".spec, "A".gen))
        .build

      run(NewtypeGen.generate(n).map(_.render(cfg.lineWidth)), cfg) must beEqvTo(
        """export type Result<A> = Either<string, A> & {__brand: "Result<A>"}""".asRight
      )
    }

    "handle upper bounds" >> {

      val n = newtype("PetsList")
        .generic("P".genExt("Pet".spec, "FourLegged".spec))
        .baseType("Array".spec("P".gen))
        .build

      run(NewtypeGen.generate(n).map(_.render(cfg.lineWidth)), cfg) must beEqvTo(
        """export type PetsList<P extends Pet & FourLegged> = Array<P> & {__brand: "PetsList<P extends Pet & FourLegged>"}""".asRight
      )
    }

  }
}
