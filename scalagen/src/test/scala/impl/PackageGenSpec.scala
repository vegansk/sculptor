package sculptor.scalagen
package impl

import org.specs2._
import cats.implicits._
import org.typelevel.paiges._

object PackageGenSpec extends mutable.Specification
    with ScalaCheck
    with testing.CatsEqMatcher {

  import testing.paiges._
  import sculptor.ast._
  import dsl._

  val cfg = Config()

  "PackageGen" should {

    val p = pkg("test").build

    "produce package" >> {
      run(PackageGen.generate(p), cfg) must beEqvTo(
        Doc.text("package test").asRight
      )
    }

    "add prefix code" >> {
      val prefix = """|/* Comment */
                      |import cats._
                      |""".stripMargin
      run(PackageGen.generate(p).map(_.render(cfg.lineWidth)), cfg.copy(prefixCode = prefix)) must beEqvTo(
        """|package test
           |
           |/* Comment */
           |import cats._
           |""".stripMargin.asRight
      )
    }

  }

}
