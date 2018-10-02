package sculptor.scalagen
package impl

import org.specs2._
import cats.implicits._
import org.typelevel.paiges._

object NewtypeGenSpec extends mutable.Specification
    with ScalaCheck
    with testing.CatsEqMatcher {

  import testing.paiges._
  import sculptor.ast._
  import dsl._

  val cfg = Config()

  "NewtypeGen" should {

    "handle simple aliases" >> {

      val n = newtype("MyInt")
        .baseType("Int".spec)
        .build

      run(NewtypeGen.generate(n), cfg) must beEqvTo(
        Doc.text("final case class MyInt(value: Int) extends AnyVal").asRight
      )

    }

    "handle generic aliases" >> {

      val n = newtype("Result")
        .generic("A".gen)
        .baseType("Either".spec("String".spec, "A".gen))
        .build

      run(NewtypeGen.generate(n), cfg) must beEqvTo(
        Doc.text("final case class Result[A](value: Either[String, A]) extends AnyVal").asRight
      )
    }

    "handle upper bounds" >> {

      val n = newtype("PetsList")
        .generic("P".genExt("Pet".spec, "FourLegged".spec))
        .baseType("List".spec("P".gen))
        .build

      run(NewtypeGen.generate(n), cfg) must beEqvTo(
        Doc.text("final case class PetsList[P <: Pet with FourLegged](value: List[P]) extends AnyVal").asRight
      )
    }

    "generate Eq typeclass" >> {
      val n = newtype("EqInt")
        .baseType("Int".spec)
        .build

      run(NewtypeGen.generate(n), cfg.copy(features = List(Feature.CatsEqTypeclass))) must beEqvTo(
        Doc.text("""|final case class EqInt(value: Int) extends AnyVal
                    |
                    |object EqInt {
                    |  implicit val EqIntEq: Eq[EqInt] = Eq.fromUniversalEquals
                    |}""".stripMargin).asRight
      )
    }

    "generate circe codecs" >> {
      val n = newtype("EqInt")
        .baseType("Int".spec)
        .build

      run(NewtypeGen.generate(n).map(_.render(cfg.lineWidth)), cfg.copy(features = List(Feature.CirceCodecs()))) must beEqvTo(
        s"""|final case class EqInt(value: Int) extends AnyVal
            |
            |object EqInt {
            |  implicit val EqIntEncoder: Encoder[EqInt] = Encoder[Int].contramap(_.value)
            |
            |  implicit val EqIntDecoder: Decoder[EqInt] = Decoder[Int].map(EqInt(_))
            |}""".stripMargin.asRight
      )
    }

  }
}
