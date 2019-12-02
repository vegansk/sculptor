package sculptor.scalagen
package impl

import org.specs2._
import cats.implicits._

object NewtypeGenSpec
    extends mutable.Specification
    with ScalaCheck
    with testing.Helpers {

  import sculptor.ast._
  import dsl._

  val cfg = Config(generateComments = false)

  "NewtypeGen" should {

    val myInt = newtype("MyInt")
      .baseType("Int".spec)
      .comment("The Int type")
      .build

    "handle simple aliases" >> {

      runGen(NewtypeGen.generate(myInt), cfg) must beEqvTo(
        "final case class MyInt(value: Int) extends AnyVal".fix.asRight
      )

    }

    "handle generic aliases" >> {

      val n = newtype("Result")
        .generic("A".gen)
        .baseType("Either".spec("String".spec, "A".gen))
        .build

      runGen(NewtypeGen.generate(n), cfg) must beEqvTo(
        "final case class Result[A](value: Either[String, A]) extends AnyVal".fix.asRight
      )
    }

    "handle upper bounds" >> {

      val n = newtype("PetsList")
        .generic("P".genExt("Pet".spec, "FourLegged".spec))
        .baseType("List".spec("P".gen))
        .build

      runGen(NewtypeGen.generate(n), cfg) must beEqvTo(
        "final case class PetsList[P <: Pet with FourLegged](value: List[P]) extends AnyVal".fix.asRight
      )
    }

    "generate Eq typeclass" >> {
      runGen(
        NewtypeGen.generate(myInt),
        cfg.copy(features = List(Feature.CatsEqTypeclass))
      ) must beEqvTo(
        """|final case class MyInt(value: Int) extends AnyVal
           |
           |object MyInt {implicit val MyIntEq: Eq[MyInt] = Eq.fromUniversalEquals}""".fix.asRight
      )
    }

    "generate circe codecs" >> {
      runGen(
        NewtypeGen.generate(myInt),
        cfg.copy(features = List(Feature.CirceCodecs()))
      ) must beEqvTo(
        """|final case class MyInt(value: Int) extends AnyVal
           |
           |object MyInt {
           |  implicit val MyIntEncoder: Encoder[MyInt] = Encoder[Int].contramap(_.value)
           |
           |  implicit val MyIntDecoder: Decoder[MyInt] = Decoder[Int].map(MyInt(_))
           |}""".fix.asRight
      )
    }

    "generate comments" >> {
      runGen(NewtypeGen.generate(myInt), cfg.copy(generateComments = true)) must beEqvTo(
        """|// Newtype MyInt: The Int type
           |
           |final case class MyInt(value: Int) extends AnyVal""".fix.asRight
      )
    }
  }
}
