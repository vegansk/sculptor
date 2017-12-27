package sculptor
package tsgen
package testing

import cats._
import org.specs2.matcher._

trait CatsEqMatcher { outer =>

  def beEqvTo[T: Eq: Show](expected: T): Matcher[T] = new Matcher[T] {
    def apply[S <: T](actual: Expectable[S]): MatchResult[S] = {
      val actualT   = actual.value.asInstanceOf[T]
      def test      = Eq[T].eqv(expected, actualT)
      def koMessage = "%s =!= %s".format(Show[T].show(actualT), Show[T].show(expected))
      def okMessage = "%s === %s".format(Show[T].show(actualT), Show[T].show(expected))
      Matcher.result(test, okMessage, koMessage, actual)
    }
  }
}
