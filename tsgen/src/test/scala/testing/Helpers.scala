package sculptor
package tsgen
package testing

import cats._
import cats.syntax.show._
import org.typelevel.paiges._
import org.specs2.matcher._
import org.specs2.matcher.describe.Diffable
import org.specs2.matcher.describe.LinesDiffable._
import common.StringOps

trait Helpers extends StringOps { outer =>

  def runFeature(r: sculptor.tsgen.impl.Result[List[Doc]],
                 cfg: Config): sculptor.tsgen.Result[String] =
    run(
      r.map(
        l => Doc.intercalate(Doc.hardLine * 2, l).render(cfg.lineWidth).fix
      ),
      cfg
    )

  def runGen(r: sculptor.tsgen.impl.Result[Doc],
             cfg: Config): sculptor.tsgen.Result[String] =
    run(r.map(_.render(cfg.lineWidth)), cfg)

  def render(doc: Doc): String = doc.render(80).fix

  def beEqvTo[T: Eq: Show](expected: T): Matcher[T] = new Matcher[T] {
    def apply[S <: T](actual: Expectable[S]): MatchResult[S] = {
      val actualT = actual.value.asInstanceOf[T]
      def test = Eq[T].eqv(expected, actualT)
      def koMessage = {
        val diff = Diffable.diff(actualT.show, expected.show)
        val diffStr = if (diff.identical) {
          "String representations match!"
        } else {
          "Diff:\n" + diff.render
        }

        "Values don't match!\n" + diffStr
      }
      def okMessage = "Values match"
      Matcher.result(test, okMessage, koMessage, actual)
    }
  }
}
