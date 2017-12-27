package sculptor
package tsgen
package testing

import org.typelevel.paiges._
import cats._

object paiges {

  implicit val docEquiv: Equiv[Doc] = new Equiv[Doc] {
    def equiv(x: Doc, y: Doc): Boolean = {
      Doc.equivAtWidths(List(x.maxWidth max y.maxWidth)).equiv(x, y)
    }
  }

  implicit val docEq: Eq[Doc] = new Eq[Doc] {
    def eqv(x: Doc, y: Doc): Boolean = Equiv[Doc].equiv(x, y)
  }

  implicit val docShow: Show[Doc] = new Show[Doc] {
    def show(d: Doc): String = d.render(80)
  }

}
