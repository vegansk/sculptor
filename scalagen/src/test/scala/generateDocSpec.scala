package sculptor.scalagen

import org.specs2._
import cats.implicits._
import org.typelevel.paiges._

object generateDocSpec
    extends mutable.Specification
    with ScalaCheck
    with testing.Helpers {

  val simpleCfg = Config(tabSize = 2)

  "scalagen.generateDoc" should {

    import testing.paiges._
    import sculptor.ast.dsl._

    "handle empty package" >> {

      val p = pkg("scalagen.generateDoc.test").build

      generateDoc(p, simpleCfg) must beEqvTo(
        Doc.text("""|package scalagen.generateDoc.test
                    |""".stripMargin).asRight
      )

    }

  }
}
