package sculptor
package iots
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
        Ident("T")
      )
    )
    import gen._

    def mkRef(t: TypeName): TypeRef = TypeRef(
      t,
      t match {
        case TypeName.std(v) => QName.of(Ident("t"), v)
        case TypeName.custom(QName(v)) => QName(NEL.fromListUnsafe(v.init ++ List(Ident(v.last.value + "Type"))))
      }
    )

    "handle qualified name" >> {

      val expr = QName(NEL.of(Ident("a"), Ident("b"), Ident("c")))
      qName(expr) must beEqvTo(Doc.text("a.b.c"))
    }

    "handle type names" >> {

      typeConst(mkRef(TypeName.std(Ident("string")))) must beEqvTo(Doc.text("t.string"))
      typeConst(mkRef(TypeName.custom(QName.of(Ident("package"), Ident("string"))))) must beEqvTo(Doc.text("package.stringType"))

    }

    "handle field declaration" >> {

      val f = FieldDecl(Ident("theField"), mkRef(TypeName.std(Ident("string"))), FieldConstraint.Required, false)
      fieldDecl(f) must beEqvTo(Doc.text("theField: t.string"))

    }

    val ct = ComplexTypeDecl(
      Ident("Test"),
      Ident("TestType"),
      None,
      true,
      NEL.of(
        FieldDecl(Ident("id"), mkRef(TypeName.std(Ident("number"))), FieldConstraint.Optional, false),
        FieldDecl(Ident("str"), mkRef(TypeName.std(Ident("string"))), FieldConstraint.Required, false),
        FieldDecl(Ident("date"), mkRef(TypeName.custom(QName.of(Ident("T"), Ident("date")))), FieldConstraint.Required, false)
      )
    )

    "handle complex type const declaration" >> {

      complexTypeConstDecl(ct) must beEqvTo(
        Doc.text(
          "export const TestType = t.intersection([" +
            "t.interface({str: t.string, date: T.dateType}), " +
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
      Ident("TestEnum"),
      Ident("TestEnumType"),
      true,
      NEL.of(
        EnumMemberDecl(Ident("V_01"), "01"),
        EnumMemberDecl(Ident("V_02"), "02")
      )
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
          """export const TestEnumType = mkStringEnum<TestEnum>(TestEnum, "TestEnum")"""
        )
      )

    }

    val imports = ImportsDecl(
      NEL.of(
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
        importsDecl(imports) + Doc.line * 2 +
          inlineMkStringEnum + Doc.line * 2 +
          complexTypeDecl(ct) + Doc.line * 2 +
          enumDecl(e)
      )

    }

  }

}
