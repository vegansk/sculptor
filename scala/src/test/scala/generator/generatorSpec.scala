package sculptor
package scala
package generator

import org.specs2._
import cats.implicits._
import cats.data.{NonEmptyList => NEL}
import org.typelevel.paiges._

object generatorSpec extends mutable.Specification
    with ScalaCheck
    with testing.CatsEqMatcher {

  "scala generator" should {

    import ast._
    import sculptor.scala.testing.paiges._

    val gen = generator.create(
      generator.Config(
        "com.github.vegansk".some,
        "/* header */".some,
        generator.Parameters(
          generateComments = false,
          generateCatsEq = true
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
                    |  lazy val NewStringEq: Eq[NewString] = Eq.fromUniversalEquals
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
                    |  lazy val values = Set[Test](
                    |    A,
                    |    B
                    |  )
                    |
                    |  val fromString: String => Option[Test] = {
                    |    s => values.find(_.code == s)
                    |  }
                    |
                    |  lazy val TestEq: Eq[Test] = Eq.fromUniversalEquals
                    |}""".stripMargin
        )
      )
    }

    val ct = ComplexTypeDecl(
      TypeRef.definedFrom("Test"),
      None,
      NEL.of(
        FieldDecl(Ident("id"), TypeRef.std(Ident("Int")), FieldConstraint.Optional, false, None),
        FieldDecl(Ident("str"), TypeRef.std(Ident("String")), FieldConstraint.Required, false, None),
        FieldDecl(Ident("date"), TypeRef.external(QName.of(Ident("Instant"))), FieldConstraint.Required, false, None)
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
             |  lazy val TestEq: Eq[Test] = Eq.fromUniversalEquals
             |}""".stripMargin
        )
      )
    }

  }

}
