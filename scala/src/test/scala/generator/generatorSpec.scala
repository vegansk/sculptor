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
        false
      )
    )
    import gen._

    "handle newtype" >> {

      val t = NewtypeDecl(TypeRef.definedFrom("NewString"), TypeRef.std(Ident("String")), None)
      newtypeDecl(t) must beEqvTo(Doc.text("type NewString = String"))
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
      enumObjDecl(e) must beEqvTo(
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
                    |    s => values.find(_.code === s)
                    |  }
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
             |)""".stripMargin
        )
      )
    }

    // "handle type names" >> {

    //   typeConst(TypeRef.std(Ident("string"))) must beEqvTo(Doc.text("t.string"))
    //   typeConst(TypeRef.external(QName.of(Ident("p"), Ident("string")), QName.of(Ident("p"), Ident("stringType")))) must beEqvTo(Doc.text("p.stringType"))

    // }

    // "handle complex type const declaration" >> {

    //   complexTypeConstDecl(ct) must beEqvTo(
    //     Doc.text(
    //       "export const TestType = t.intersection([" +
    //         "t.interface({str: t.string, date: T.date}), " +
    //         "t.partial({id: t.number})" +
    //         "], \"Test\")"
    //     )
    //   )

    // }

    // "handle complex type interface declaration" >> {

    //   complexTypeIntfDecl(ct) must beEqvTo(
    //     Doc.text("export interface Test extends t.TypeOf<typeof TestType> {}")
    //   )

    // }

    // val e = EnumDecl(
    //   TypeRef.definedFrom("TestEnum", "TestEnumType"),
    //   NEL.of(
    //     EnumMemberDecl(Ident("V_01"), "01", None),
    //     EnumMemberDecl(Ident("V_02"), "02", None)
    //   ),
    //   None
    // )

    // "handle enum type declaration" >> {

    //   enumTypeDecl(e) must beEqvTo(
    //     Doc.text(
    //       """export enum TestEnum {V_01 = "01", V_02 = "02"}"""
    //     )
    //   )

    // }

    // "handle enum constant declaration" >> {

    //   enumConstDecl(e) must beEqvTo(
    //     Doc.text(
    //       """export const TestEnumType = mkStringEnum<TestEnum>(TestEnum, "TestEnum")"""
    //     )
    //   )

    // }

    // val imports = ImportsDecl(
    //   List(
    //     ImportDecl("java.time.Instant")
    //   )
    // )

    // "handle imports declaration" >> {

    //   importsDecl(imports) must beEqvTo(
    //     Doc.text("import java.time.Instant")
    //   )

    // }

    // val module = ModuleDecl(
    //   imports.some,
    //   TypesDecl(NEL.of(ct, e)).some
    // )

    // "handle module declaration" >> {

    //   moduleDecl(module) must beEqvTo(
    //     Doc.text("/* header */") + Doc.line * 2 +
    //     importsDecl(imports) + Doc.line * 2 +
    //       complexTypeDecl(ct) + Doc.line * 2 +
    //       enumDecl(e)
    //   )

    // }

  }

}
