package sculptor.tsgen
package impl

import cats.implicits._
import org.specs2._

object ADTGenSpec extends mutable.Specification
    with ScalaCheck
    with testing.Helpers {

  import sculptor.ast._
  import dsl._

  val cfg = Config(generateComments = false, generateAdtNs = false)

  "ADTGen" should {

    val maybeAdt = adt("Maybe")
      .generic("A".gen)
      .constructors(
        cons("Empty").generic("A".gen).comment("The empty value"),
        cons("Just")
          .generic("A".gen)
          .field("value", "A".gen, comment = "The value")
          .comment("The non empty value")
      )
      .comment("The type representing optional value")
      .build

    "generate ADTs" >> {
      runGen(ADTGen.generate(maybeAdt), cfg) must beEqvTo(
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

    "handle optional encoding" >> {
      runGen(ADTGen.generate(maybeAdt), cfg.copy(optionalEncoding = OptionalEncoding(allFieldsOptional = true))) must beEqvTo(
        """|export type Maybe<A> = Empty<A> | Just<A>
           |
           |export interface Empty<A> {__tag: "Empty<A>"}
           |
           |export interface Just<A> {
           |  __tag: "Just<A>"
           |  value?: A
           |}""".stripMargin.asRight
      )
    }

    "generate comments" >> {
      runGen(ADTGen.generate(maybeAdt), cfg.copy(generateComments = true)) must beEqvTo(
        """|// ADT Maybe<A>: The type representing optional value
           |
           |export type Maybe<A> = Empty<A> | Just<A>
           |
           |/* The empty value */
           |export interface Empty<A> {__tag: "Empty<A>"}
           |
           |/* The non empty value */
           |export interface Just<A> {
           |  __tag: "Just<A>"
           |  value: A /* The value */
           |}""".stripMargin.asRight
      )
    }

    "generate namespace for ATD constructors" >> {
      runGen(ADTGen.generate(maybeAdt), cfg.copy(generateAdtNs = true)) must beEqvTo(
        """|export type Maybe<A> = Maybe.Empty<A> | Maybe.Just<A>
           |
           |export namespace Maybe {
           |  export interface Empty<A> {__tag: "Maybe.Empty<A>"}
           |
           |  export interface Just<A> {
           |    __tag: "Maybe.Just<A>"
           |    value: A
           |  }
           |}""".stripMargin.asRight
      )
    }
  }

}
