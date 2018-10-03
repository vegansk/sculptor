package sculptor.tsgen
package impl

import cats.implicits._
import org.specs2._

object ADTGenSpec extends mutable.Specification
    with ScalaCheck
    with testing.CatsEqMatcher {

  import sculptor.ast._
  import dsl._

  val cfg = Config()

  "ADTGen" should {

    "generate ADTs" >> {

      val a = adt("Maybe")
        .generic("A".gen)
        .constructors(
          cons("Empty").generic("A".gen),
          cons("Just")
            .generic("A".gen)
            .field("value", "A".gen)
        )
        .build

      run(ADTGen.generate(a).map(_.render(cfg.lineWidth)), cfg) must beEqvTo(
        """|export type Maybe<A> = Empty<A> | Just<A>
           |
           |export interface Empty<A> {__tag: "Empty<A>"}
           |
           |export interface Just<A> {
           |  __tag: "Just<A>"
           |  value: A
           |}""".stripMargin.asRight
      )
    }

  }

}
