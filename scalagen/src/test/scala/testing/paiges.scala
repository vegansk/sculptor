package sculptor
package scalagen
package testing

import org.typelevel.paiges._
import cats._

object paiges {

  implicit val docEq: Eq[Doc] = new Eq[Doc] {
    // TODO: Remove this WTF!
    def eqv(x: Doc, y: Doc): Boolean = {
      val x0 = x.render(0).split("\\n").map(_.trim).mkString("\n")
      val y0 = y.render(0).split("\\n").map(_.trim).mkString("\n")
      if(x0 != y0) {
        x0.split("\\n").foreach(v => println(s"*$v*"))
        y0.split("\\n").foreach(v => println(s"*$v*"))
      }
      x0 == y0
    }
  }

  implicit val docShow: Show[Doc] = new Show[Doc] {
    def show(d: Doc): String = d.render(80)
  }

}
