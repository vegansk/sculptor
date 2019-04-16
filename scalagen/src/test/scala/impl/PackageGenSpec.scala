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

    val p = pkg("test").comment("Package comment").build

    "produce package" >> {
      runGen(PackageGen.generate(p), cfg) must beEqvTo("package test".asRight)
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
           |""".stripMargin.asRight
      )
    }

    "generate comments" >> {
      runGen(PackageGen.generate(p), cfg.copy(generateComments = true)) must beEqvTo(
        """|/*
           |Package comment
           |*/
           |
           |package test""".stripMargin.asRight
      )
    }

  }

}
