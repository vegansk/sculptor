package sculptor.scalagen
package testing

import org.typelevel.paiges._
import cats._

object paiges {

  implicit val docEq: Eq[Doc] = new Eq[Doc] {
    // TODO: Find a better way to compare Docs
    def eqv(x: Doc, y: Doc): Boolean = {
      val x0 = x
        .render(80)
        .split("\\n")
        .map(_.trim)
        .filterNot(_.isEmpty)
        .mkString("")
        .trim
      val y0 = y
        .render(80)
        .split("\\n")
        .map(_.trim)
        .filterNot(_.isEmpty)
        .mkString("")
        .trim
      if (x0 != y0) {
        println(s"*$x0*")
        println(s"*$y0*")
      }
      x0 == y0
    }
  }

  implicit val docShow: Show[Doc] = new Show[Doc] {
    def show(d: Doc): String = d.render(80)
  }

}
