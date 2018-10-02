package sculptor.scalagen.deprecated
package generator

import org.specs2._
import cats.implicits._
import cats.data.{NonEmptyList => NEL}
import org.typelevel.paiges._

object generatorSpec extends mutable.Specification
    with ScalaCheck
    with sculptor.scalagen.testing.CatsEqMatcher {

  "scala generator" should {

    import ast._
    import sculptor.scalagen.testing.paiges._

    val gen = generator.create(
      generator.Config(
        "com.github.vegansk".some,
        "/* header */".some,
        Nil,
        generator.Parameters(
          generateComments = false,
          generateCatsEq = true,
          generateCirceCodecs = true,
          generateXmlSerializers = false
        )
      )
    )
    import gen._

    "handle newtype" >> {

      val t = NewtypeDecl(TypeRef.definedFrom("NewString"), TypeRef.std(Ident("String")), None)
      newtypeDecl(t) must beEqvTo(
        Doc.text("""|final case class NewString(
                    |  value: String
                    |)
                    |object NewString {
                    |  implicit val NewStringEq: Eq[NewString] = Eq.fromUniversalEquals
                    |
                    |  implicit val NewStringEncoder: Encoder[NewString] = Encoder[String].contramap(_.value)
                    |
                    |  implicit val NewStringDecoder: Decoder[NewString] = Decoder[String].map(NewString(_))
                    |}""".stripMargin)
      )
    }

    "handle enums" >> {
      val e = EnumDecl(
        TypeRef.definedFrom("Test"),
        NEL.of(
          EnumMemberDecl(Ident("A"), "valueA", Some("Value A")),
          EnumMemberDecl(Ident("B"), "valueB", None),
        ),
        None
      )

      enumTypeDecl(e) must beEqvTo(
        Doc.text("""|sealed trait Test {
                    |  val code: String
                    |  val description: String
                    |}""".stripMargin)
      )
      enumObjectDecl(e) must beEqvTo(
        Doc.text("""|object Test {
                    |  object A extends Test {
                    |    override val code = "valueA"
                    |    override val description = "Value A"
                    |  }
                    |  object B extends Test {
                    |    override val code = "valueB"
                    |    override val description = "valueB"
                    |  }
                    |
                    |  lazy val values = Set[Test](A, B)
                    |
                    |  val fromString: String => Option[Test] = {
                    |    s => values.find(_.code === s)
                    |  }
                    |
                    |  implicit val TestEq: Eq[Test] = Eq.fromUniversalEquals
                    |
                    |  implicit val TestEncoder: Encoder[Test] = Encoder[String].contramap(_.code)
                    |
                    |  implicit val TestDecoder: Decoder[Test] = Decoder[String].emap(fromString(_).toRight("Invalid enum value"))
                    |}""".stripMargin
        )
      )
    }

    val ct = ComplexTypeDecl(
      TypeRef.definedFrom("Test"),
      None,
      NEL.of(
        FieldDecl(Ident("id"), "id", TypeRef.std(Ident("Int")), FieldConstraint.Optional, false, false, None),
        FieldDecl(Ident("str"), "str", TypeRef.std(Ident("String")), FieldConstraint.Required, false, false, None),
        FieldDecl(Ident("date"), "date", TypeRef.external(QName.of(Ident("Instant"))), FieldConstraint.Required, false, false, None)
      ),
      None
    )

    "handle complex types" >> {
      complexTypeDecl(ct) must beEqvTo(
        Doc.text(
          """|final case class Test(
             |  id: Option[Int],
             |  str: String,
             |  date: Instant
             |)
             |object Test {
             |  implicit val TestEq: Eq[Test] = Eq.fromUniversalEquals
             |
             |  implicit val TestObjectEncoder: ObjectEncoder[Test] = ObjectEncoder.instance[Test] { v =>
             |    JsonObject(
             |      "id" := v.id,
             |      "str" := v.str,
             |      "date" := v.date
             |    )
             |  }
             |
             |  implicit val TestDecoder: Decoder[Test] = Decoder.instance[Test] { c =>
             |    for {
             |      id <- c.downField("id").as[Option[Int]]
             |      str <- c.downField("str").as[String]
             |      date <- c.downField("date").as[Instant]
             |    } yield Test(id, str, date)
             |  }
             |}""".stripMargin
        )
      )
    }

  }

}