package sculptor.scalagen.deprecated
package generator

import org.specs2._
import cats.implicits._
import cats.data.{NonEmptyList => NEL}
import org.typelevel.paiges._

object generatorSpec
    extends mutable.Specification
    with ScalaCheck
    with sculptor.scalagen.testing.Helpers {

  object data {
    import ast._

    val genConfig = generator.Config(
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

    val newtypeDecl = NewtypeDecl(
      TypeRef.definedFrom("NewString"),
      TypeRef.std(Ident("String")),
      None
    )

    val enumDecl = EnumDecl(
      TypeRef.definedFrom("Test"),
      NEL.of(
        EnumMemberDecl(Ident("A"), "valueA", Some("Value A")),
        EnumMemberDecl(Ident("B"), "valueB", None),
      ),
      None
    )

    val complexTypeDecl = ComplexTypeDecl(
      TypeRef.definedFrom("Test"),
      None,
      NEL.of(
        FieldDecl(
          Ident("id"),
          "id",
          TypeRef.std(Ident("Int")),
          FieldConstraint.Optional,
          false,
          false,
          None
        ),
        FieldDecl(
          Ident("str"),
          "str",
          TypeRef.std(Ident("String")),
          FieldConstraint.Required,
          false,
          false,
          None
        ),
        FieldDecl(
          Ident("date"),
          "date",
          TypeRef.external(QName.of(Ident("Instant"))),
          FieldConstraint.Required,
          false,
          false,
          None
        )
      ),
      None
    )

    val complexTypeWithDocDecl = ComplexTypeDecl(
      TypeRef.definedFrom("Test"),
      None,
      NEL.of(
        FieldDecl(
          Ident("id"),
          "id",
          TypeRef.std(Ident("Int")),
          FieldConstraint.Optional,
          false,
          false,
          "The id field".some
        ),
        FieldDecl(
          Ident("str"),
          "str",
          TypeRef.std(Ident("String")),
          FieldConstraint.Required,
          false,
          false,
          "The str field".some
        )
      ),
      "Complex type for test".some
    )
  }

  "scala generator" should {

    import sculptor.scalagen.testing.paiges._

    val gen = generator.create(data.genConfig)
    import gen._

    "handle newtype" >> {

      newtypeDecl(data.newtypeDecl) must beEqvTo(
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

      enumTypeDecl(data.enumDecl) must beEqvTo(
        Doc.text("""|sealed trait Test extends Product with Serializable {
                    |  val code: String
                    |  val description: String
                    |}""".stripMargin)
      )
      enumObjectDecl(data.enumDecl) must beEqvTo(
        Doc.text("""|object Test {
                    |  case object A extends Test {
                    |    override val code = "valueA"
                    |    override val description = "Value A"
                    |  }
                    |  case object B extends Test {
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
                    |}""".stripMargin)
      )
    }

    "handle complex types" >> {
      complexTypeDecl(data.complexTypeDecl) must beEqvTo(
        Doc.text("""|final case class Test(
             |  id: Option[Int],
             |  str: String,
             |  date: Instant
             |)
             |object Test {
             |  implicit val TestEq: Eq[Test] = Eq.fromUniversalEquals
             |
             |  implicit val TestEncoder: Encoder.AsObject[Test] = Encoder.AsObject.instance[Test] { v =>
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
             |}""".stripMargin)
      )
    }

    "generate scaladoc for complex types" >> {

      val gen0 = generator.create(
        data.genConfig.copy(
          parameters = generator.Parameters(
            generateComments = true,
            generateCatsEq = false,
            generateCirceCodecs = false,
            generateXmlSerializers = false,
            generateKantanXPathDecoders = false,
            generateOptionalTypes = generator.OptionalTypes.No
          )
        )
      )

      gen0.complexTypeDecl(data.complexTypeWithDocDecl) must beEqvTo(
        Doc.text("""|/** Complex type for test
                    |  * @param id The id field
                    |  * @param str The str field
                    |  */
                    |final case class Test(
                    |  id: Option[Int],
                    |  str: String
                    |)
                    |""".stripMargin)
      )
    }

    "generate parameters default values for complex types" >> {

      "producing strong types" >> {
        val gen0 = generator.create(
          data.genConfig.copy(
            parameters = generator.Parameters(
              generateComments = true,
              generateCatsEq = false,
              generateCirceCodecs = false,
              generateXmlSerializers = false,
              generateKantanXPathDecoders = false,
              generateOptionalTypes = generator.OptionalTypes.No,
              generateParametersDefaultValues = true
            )
          )
        )

        gen0.complexTypeDecl(data.complexTypeDecl) must beEqvTo(
          Doc.text("""|final case class Test(
              |  id: Option[Int] = None,
              |  str: String,
              |  date: Instant
              |)""".stripMargin)
        )
      }

      "producing optional types" >> {
        val gen0 = generator.create(
          data.genConfig.copy(
            parameters = generator.Parameters(
              generateComments = true,
              generateCatsEq = false,
              generateCirceCodecs = false,
              generateXmlSerializers = false,
              generateKantanXPathDecoders = false,
              generateOptionalTypes = generator.OptionalTypes.Generate(),
              generateParametersDefaultValues = true
            )
          )
        )

        gen0.complexTypeDecl(data.complexTypeDecl) must beEqvTo(
          Doc.text("""|final case class Test(
              |  id: Option[Int] = None,
              |  str: Option[String] = None,
              |  date: Option[Instant] = None
              |)""".stripMargin)
        )
      }
    }
  }

}
