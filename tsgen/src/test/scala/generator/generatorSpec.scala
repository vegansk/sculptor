package sculptor
package tsgen
package generator

import org.specs2._
import cats.implicits._
import cats.data.{NonEmptyList => NEL}
import org.typelevel.paiges._

object generatorSpec extends mutable.Specification
    with ScalaCheck
    with testing.CatsEqMatcher {

  "iots generator" should {

    import ast._
    import testing.paiges._

    val gen = generator.create(
      generator.Config(
        Ident("t"),
        "/* header */".some,
        false,
        false,
        false,
        false,
        false
      )
    )
    import gen._

    "handle qualified name" >> {

      val expr = QName(NEL.of(Ident("a"), Ident("b"), Ident("c")))
      qName(expr) must beEqvTo(Doc.text("a.b.c"))
    }

    "handle type names" >> {

      typeConst(TypeRef.std(Ident("string"))) must beEqvTo(Doc.text("t.string"))
      typeConst(TypeRef.external(QName.of(Ident("p"), Ident("string")), QName.of(Ident("p"), Ident("stringType")))) must beEqvTo(Doc.text("p.stringType"))

    }

    "handle field declaration" >> {

      val f = FieldDecl(Ident("theField"), TypeRef.std(Ident("string")), FieldConstraint.Required, false, None)
      fieldDecl(f) must beEqvTo(Doc.text("theField: t.string"))

    }

    val ct = ComplexTypeDecl(
      TypeRef.definedFrom("Test", "TestType"),
      None,
      true,
      NEL.of(
        FieldDecl(Ident("id"), TypeRef.std(Ident("number")), FieldConstraint.Optional, false, None),
        FieldDecl(Ident("str"), TypeRef.std(Ident("string")), FieldConstraint.Required, false, None),
        FieldDecl(Ident("date"), TypeRef.external(QName.of(Ident("Date")), QName.of(Ident("T"), Ident("date"))), FieldConstraint.Required, false, None)
      ),
      None
    )

    "handle complex type const declaration" >> {

      complexTypeConstDecl(ct) must beEqvTo(
        Doc.text(
          "export const TestType: t.Type<Test> = t.intersection([" +
            "t.interface({str: t.string, date: T.date}), " +
            "t.partial({id: t.number})" +
            "], \"Test\")"
        )
      )

    }

    "handle complex type interface declaration" >> {

      complexTypeIntfDecl(ct) must beEqvTo(
        Doc.text("export interface Test extends t.TypeOf<typeof TestType> {}")
      )

    }

    val e = EnumDecl(
      TypeRef.definedFrom("TestEnum", "TestEnumType"),
      true,
      NEL.of(
        EnumMemberDecl(Ident("V_01"), "01", None),
        EnumMemberDecl(Ident("V_02"), "02", None)
      ),
      None
    )

    "handle enum type declaration" >> {

      enumTypeDecl(e) must beEqvTo(
        Doc.text(
          """export enum TestEnum {V_01 = "01", V_02 = "02"}"""
        )
      )

    }

    "handle enum constant declaration" >> {

      enumConstDecl(e) must beEqvTo(
        Doc.text(
          """export const TestEnumType: t.Type<TestEnum> = mkStringEnum<TestEnum>(TestEnum, "TestEnum")"""
        )
      )

    }

    val imports = ImportsDecl(
      List(
        ImportDecl(Ident("t"), "io-ts"),
        ImportDecl(Ident("T"), "core/utils/types")
      )
    )

    "handle imports declaration" >> {

      importsDecl(imports) must beEqvTo(
        Doc.text(
          """import * as t from "io-ts"
import * as T from "core/utils/types""""
        )
      )

    }

    val module = ModuleDecl(
      imports.some,
      TypesDecl(NEL.of(ct, e)).some
    )

    "handle module declaration" >> {

      moduleDecl(module) must beEqvTo(
        Doc.text("/* header */") + Doc.line * 2 +
        importsDecl(imports) + Doc.line * 2 +
          inlineMkStringEnum + Doc.line * 2 +
          complexTypeDecl(ct) + Doc.line * 2 +
          enumDecl(e)
      )

    }

  }

}
