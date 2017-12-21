package sculptor
package iots
package generator

import org.specs2._
import cats.implicits._
import cats.data.{NonEmptyList => NEL}
import org.typelevel.paiges._

object nativeGeneratorSpec extends mutable.Specification
    with ScalaCheck
    with testing.CatsEqMatcher {

  "iots native generator" should {

    import ast._
    import testing.paiges._

    val gen = generator.create(
      generator.Config(
        Ident("t"),
        "/* header */".some,
        true
      )
    )
    import gen._

    val ct = ComplexTypeDecl(
      TypeRef.definedFrom("Test", "TestType"),
      TypeRef.definedFrom("Base", "BaseType").some,
      true,
      NEL.of(
        FieldDecl(Ident("id"), TypeRef.std(Ident("number")), FieldConstraint.Optional, false),
        FieldDecl(Ident("str"), TypeRef.std(Ident("string")), FieldConstraint.Required, false),
        FieldDecl(Ident("date"), TypeRef.external(QName.of(Ident("Date")), QName.of(Ident("T"), Ident("date"))), FieldConstraint.Required, false)
      )
    )

    "produce native interface declarations" >> {

      complexTypeIntfDecl(ct).grouped must beEqvTo(
        Doc.text(
          "export interface Test extends Base {" +
            "id?: number, " +
            "str: string, " +
            "date: Date" +
            "}"
        )
      )

    }
  }
}
