package sculptor
package tsgen
package testing

import cats._
import org.typelevel.paiges._
import org.specs2.matcher._
import common.StringOps

trait Helpers extends StringOps { outer =>

  def runFeature(r: sculptor.tsgen.impl.Result[List[Doc]],
                 cfg: Config): sculptor.tsgen.Result[String] =
    run(
      r.map(
        l => Doc.intercalate(Doc.lineNoFlat * 2, l).render(cfg.lineWidth).fix
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
      def koMessage =
        "%s =!= %s".format(Show[T].show(actualT), Show[T].show(expected))
      def okMessage =
        "%s === %s".format(Show[T].show(actualT), Show[T].show(expected))
      Matcher.result(test, okMessage, koMessage, actual)
    }
  }
}
