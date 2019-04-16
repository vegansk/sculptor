package sculptor
package scalagen
package testing

import cats._
import org.typelevel.paiges._
import org.specs2.matcher._

trait Helpers { outer =>

  def runFeature(r: sculptor.scalagen.impl.Result[List[Doc]],
                 cfg: Config): sculptor.scalagen.Result[String] =
    run(
      r.map(l => Doc.intercalate(Doc.lineNoFlat * 2, l).render(cfg.lineWidth)),
      cfg
    )

  def runGen(r: sculptor.scalagen.impl.Result[Doc],
             cfg: Config): sculptor.scalagen.Result[String] =
    run(r.map(_.render(cfg.lineWidth)), cfg)

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
