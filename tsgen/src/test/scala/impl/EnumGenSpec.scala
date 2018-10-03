package sculptor.tsgen
package impl

import cats.implicits._
import org.specs2._

object EnumGenSpec extends mutable.Specification
    with ScalaCheck
    with testing.CatsEqMatcher {

  import sculptor.ast._
  import dsl._

  val cfg = Config()

  "EnumGenSpec" should {

    "generate ADTs" >> {

      val e = enum("Colors")
        .values(
          enumValue("Red").value("red"),
          enumValue("Green").value("green"),
          enumValue("Blue").value("blue")
        )
        .build

      run(EnumGen.generate(e).map(_.render(cfg.lineWidth)), cfg) must beEqvTo(
        """|enum Colors {
           |  Red = "red",
           |  Green = "green",
           |  Blue = "blue"
           |}""".stripMargin.asRight
      )
    }
  }
}
