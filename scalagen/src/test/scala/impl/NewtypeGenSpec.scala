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
        """|final case class MyInt(
           |  value: Int
           |) extends AnyVal""".stripMargin.fix.asRight
      )

    }

    "handle generic aliases" >> {

      val n = newtype("Result")
        .generic("A".gen)
        .baseType("Either".spec("String".spec, "A".gen))
        .build

      runGen(NewtypeGen.generate(n), cfg) must beEqvTo(
        """|final case class Result[A](
           |  value: Either[String, A]
           |) extends AnyVal""".stripMargin.fix.asRight
      )
    }

    "handle upper bounds" >> {

      val n = newtype("PetsList")
        .generic("P".genExt("Pet".spec, "FourLegged".spec))
        .baseType("List".spec("P".gen))
        .build

      runGen(NewtypeGen.generate(n), cfg) must beEqvTo(
        """|final case class PetsList[P <: Pet with FourLegged](
           |  value: List[P]
           |) extends AnyVal""".fix.asRight
      )
    }

    "generate Eq typeclass" >> {
      runGen(
        NewtypeGen.generate(myInt),
        cfg.copy(features = List(Feature.CatsEqTypeclass))
      ) must beEqvTo("""|final case class MyInt(
           |  value: Int
           |) extends AnyVal
           |
           |object MyInt {
           |  implicit val MyIntEq: Eq[MyInt] = Eq.fromUniversalEquals
           |}""".fix.asRight)
    }

    "generate circe codecs" >> {
      runGen(
        NewtypeGen.generate(myInt),
        cfg.copy(features = List(Feature.CirceCodecs()))
      ) must beEqvTo(
        """|final case class MyInt(
           |  value: Int
           |) extends AnyVal
           |
           |object MyInt {
           |  implicit val MyIntEncoder: Encoder[MyInt] = Encoder[Int].contramap(_.value)
           |  
           |  implicit val MyIntDecoder: Decoder[MyInt] = Decoder[Int].map(MyInt(_))
           |}""".fix.asRight
      )
    }

    "generate tapir Schema" >> {
      runGen(
        NewtypeGen.generate(myInt),
        cfg.copy(features = List(Feature.TapirSchema()))
      ) must beEqvTo("""|final case class MyInt(
           |  value: Int
           |) extends AnyVal
           |
           |object MyInt {
           |  implicit val MyIntSchema: Schema[MyInt] =
           |    implicitly[Schema[Int]]
           |      .map(x => Some(MyInt(x)))(_.value)
           |      .description("The Int type")
           |      .name(Schema.SName("MyInt"))
           |}""".fix.asRight)
    }

    "generate comments" >> {
      runGen(NewtypeGen.generate(myInt), cfg.copy(generateComments = true)) must beEqvTo(
        """|// Newtype MyInt:
           |// The Int type
           |
           |final case class MyInt(
           |  value: Int
           |) extends AnyVal""".fix.asRight
      )
    }

    "escape reserved words" >> {
      val a = newtype("case")
        .generic(GenericDef.of("P", TypeRef.spec("val"), TypeRef.spec("class")))
        .baseType(TypeRef.spec("object", TypeRef.gen("P")))
        .build

      // TODO: Should be changed after issue #256
      runGen(
        NewtypeGen.generate(a),
        cfg.copy(
          features = List(
            Feature.TapirSchema(),
            Feature.CatsEqTypeclass,
            Feature.CirceCodecs()
          )
        )
      ) must beEqvTo(
        """|final case class `case`[P <: `val` with `class`](
          |  value: `object`[P]
          |) extends AnyVal
          |
          |object `case` {
          |  implicit def caseSchema[P:Schema]: Schema[`case`[P]] =
          |    implicitly[Schema[`object`[P]]]
          |      .map(x => Some(`case`(x)))(_.value)
          |      .name(Schema.SName("case"))
          |  
          |  implicit def caseEq[P]: Eq[`case`[P]] = Eq.fromUniversalEquals
          |  
          |  implicit def caseEncoder[P:Encoder]: Encoder[`case`[P]] = Encoder[`object`[P]].contramap(_.value)
          |  
          |  implicit def caseDecoder[P:Decoder]: Decoder[`case`[P]] = Decoder[`object`[P]].map(`case`(_))
          |}""".fix.asRight
      )
    }
  }
}
