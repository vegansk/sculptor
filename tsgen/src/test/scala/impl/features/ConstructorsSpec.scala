package sculptor.tsgen
package impl
package features

import org.specs2._
import cats.implicits._
import org.typelevel.paiges._

object ConstructorsSpec extends mutable.Specification
    with ScalaCheck
    with testing.CatsEqMatcher {

  import sculptor.ast._
  import dsl._

  val cfg = Config(features = List(Feature.Constructors))

  def runFeature(r: Result[List[Doc]], cfg: Config): sculptor.tsgen.Result[String] =
    run(r.map(l => Doc.intercalate(Doc.lineNoFlat * 2, l).render(cfg.lineWidth)), cfg)

  "Constructors" should {

    "handle newtypes" >> {
      val n = newtype("Result")
        .generic("A".gen)
        .baseType("Either".spec("string".spec, "A".gen))
        .build
      runFeature(Constructors.handleNewtype(n), cfg) must beEqvTo(
        """|export const Result = <A>(value: Either<string, A>): Result<A> => {return value as Result<A>}""".stripMargin.asRight
      )
    }

    "handle records" >> {
      val r = record("Record").generic("T".gen)
      .field("id", "T".gen)
      .field("name", "string".spec)
      .build

      runFeature(Constructors.handleRecord(r), cfg) must beEqvTo(
        """|export const Record = <T>(id: T, name: string): Record<T> => {
           |  return {
           |    id,
           |    name
           |  }
           |}""".stripMargin.asRight
      )
    }

    "handle ADTs" >> {
      val a = adt("Maybe")
        .generic("A".gen)
        .constructors(
          cons("Empty").generic("A".gen),
          cons("Just")
            .generic("A".gen)
            .field("value", "A".gen)
        )
        .build

      runFeature(Constructors.handleADT(a), cfg) must beEqvTo(
        """|export const Empty = <A>(): Maybe<A> => {return {__tag: "Empty<A>"}}
           |
           |export const Just = <A>(value: A): Maybe<A> => {
           |  return {
           |    __tag: "Just<A>",
           |    value
           |  }
           |}""".stripMargin.asRight
      )
    }
  }
}
