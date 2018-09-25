package sculptor.scalagen
package impl

import cats.implicits._
import org.specs2._
import org.typelevel.paiges._

object ADTGenSpec extends mutable.Specification
    with ScalaCheck
    with testing.CatsEqMatcher {

  import testing.paiges._
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

      run(ADTGen.generate(a), cfg) must beEqvTo(
        Doc.text("""|sealed trait Maybe[A] extends Product with Serializable
                    |
                    |object Maybe {
                    |  final case class Empty[A]() extends Maybe[A]
                    |  final case class Just[A](value: A) extends Maybe[A]
                    |}
                    |""".stripMargin).asRight
      )
    }

    "generate simple enums" >> {
      val a = adt("Colors")
        .constructors(
          cons("Red"),
          cons("Green"),
          cons("Blue")
        )
        .build

      run(ADTGen.generate(a), cfg) must beEqvTo(
        Doc.text("""|sealed trait Colors extends Product with Serializable
                    |
                    |object Colors {
                    |  case object Red extends Colors
                    |  case object Green extends Colors
                    |  case object Blue extends Colors
                    |}
                    |""".stripMargin).asRight
      )
    }

    "generate Eq typeclass" >> {
      val a = adt("Maybe")
        .generic("A".gen)
        .constructors(
          cons("Empty").generic("A".gen),
          cons("Just")
            .generic("A".gen)
            .field("value", "A".gen)
        )
        .build

      run(ADTGen.generate(a), cfg.copy(features = List(Feature.CatsEqTypeclass))) must beEqvTo(
        Doc.text("""|sealed trait Maybe[A] extends Product with Serializable
                    |
                    |object Maybe {
                    |  final case class Empty[A]() extends Maybe[A]
                    |  final case class Just[A](value: A) extends Maybe[A]
                    |
                    |  implicit def MaybeEq[A]: Eq[Maybe[A]] = Eq.fromUniversalEquals
                    |}
                    |""".stripMargin).asRight
      )
    }

  }

}
