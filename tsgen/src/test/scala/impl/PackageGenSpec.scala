package sculptor.tsgen
package impl

import cats.implicits._
import org.specs2._

object PackageGenSpec
    extends mutable.Specification
    with ScalaCheck
    with testing.Helpers {

  import sculptor.ast._
  import dsl._

  val cfg = Config(generateComments = false)

  val `package` = {
    val B = newtype("B").baseType("number".spec)
      .build
    val A = newtype("A").baseType("number".spec)
      .additionalDependencies(List(B))
      .build

    pkg("test").typeDefs(A, B).build
  }

  "PackageGen" should {

    "support manual dependencies" >> {
      runGen(PackageGen.generate(`package`), cfg) must beEqvTo(
        """|export type B = number & {__brand: "B"}
           |
           |export type A = number & {__brand: "A"}""".fix.asRight
      )
    }

  }
}