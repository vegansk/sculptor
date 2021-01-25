package sculptor.scalagen
package impl

import org.specs2._
import cats.implicits._

object PackageGenSpec
    extends mutable.Specification
    with ScalaCheck
    with testing.Helpers {

  import sculptor.ast._
  import dsl._

  val cfg = Config(generateComments = false)

  "PackageGen" should {

    val p = pkg("test")
      .comment("Package comment")
      .additionalCodeS("// Additional comment")
      .build

    "produce package" >> {
      runGen(PackageGen.generate(p), cfg) must beEqvTo(
        "package test".fix.asRight
      )
    }

    "add prefix code" >> {
      val prefix = """|/* Comment */
                      |import cats._
                      |""".stripMargin
      runGen(PackageGen.generate(p), cfg.copy(prefixCode = prefix)) must beEqvTo(
        """|package test
           |
           |/* Comment */
           |import cats._
           |""".fix.asRight
      )
    }

    "generate comments" >> {
      runGen(PackageGen.generate(p), cfg.copy(generateComments = true)) must beEqvTo(
        """|/*
           |Package comment
           |*/
           |
           |package test""".fix.asRight
      )
    }

    "generate additional code" >> {
      runGen(
        PackageGen.generate(p),
        cfg.copy(features = Feature.AdditionalCode.pure[List])
      ) must beEqvTo("""|package test
           |
           |// Additional comment""".fix.asRight)
    }

    "support manual dependencies" >> {
      val `package` = {
        val B = newtype("B").baseType("number".spec)
          .build
        val A = newtype("A").baseType("number".spec)
          .additionalDependencies(List(B))
          .build

        pkg("test").typeDefs(A, B).build
      }
      runGen(PackageGen.generate(`package`), cfg) must beEqvTo(
        """|package test
           |
           |final case class B(value: number) extends AnyVal
           |
           |final case class A(value: number) extends AnyVal""".fix.asRight
      )
    }

  }

}
