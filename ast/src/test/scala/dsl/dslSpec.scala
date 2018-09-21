package sculptor
package ast
package dsl

import org.specs2._
import cats.data.NonEmptyList
import cats.implicits._

object dslSpec extends mutable.Specification {

  import TypeRef.{spec, gen}

  "DSL" should {

    // final case class NewInt(value: Int) extends AnyVal
    val simpleNewType = newtype("NewInt")
      .baseType("Int".spec)
      .comment("NewInt type")

    "build simple newtypes" >> {
      simpleNewType.build must_=== Newtype(Ident("NewInt"), Nil, spec("Int"), comment = "NewInt type".some)
    }

    val genericNewType = newtype("ListOfEitherS")
      .generic("A".genExt("BaseType".spec))
      .baseType("List".spec("Either".spec("String".spec, "A".gen)))

    "build generic newtypes" >> {
      // final case class ListOfEitherS[A <: BaseType](value: List[Either[String, A]]) extends AnyVal
      genericNewType.build must_=== Newtype(
        Ident("ListOfEitherS"), List(GenericDef.of("A", spec("BaseType"))), spec("List", spec("Either", spec("String"), gen("A"))))
    }

    val simpleAlias = alias("NewInt")
      .baseType("Int".spec)
      .comment("NewInt type")

    "build simple aliases" >> {
      // type NewInt = Int
      simpleAlias.build must_=== Alias(Ident("NewInt"), Nil, spec("Int"), comment = "NewInt type".some)
    }

    val genericAlias = alias("ListOfEitherS")
      .generic("A".genExt("BaseType".spec))
      .baseType("List".spec("Either".spec("String".spec, "A".gen)))

    "build generic aliases" >> {
      // type ListOfEitherS[A <: BaseType] = List[Either[String, A]]
      genericAlias.build must_=== Alias(
        Ident("ListOfEitherS"), List(GenericDef.of("A", spec("BaseType"))), spec("List", spec("Either", spec("String"), gen("A"))))
    }

    "build field definitions" >> {
      val f = field("x")
        .ofType(spec("Int"))
        .comment("The integer")
        .build
      f must_=== FieldDef(Ident("x"), spec("Int"), comment = "The integer".some)
    }

    val rec = record("Record")
      .generic(("A".genExt("BaseType".spec)))
      .fields(field("x").ofType("A".gen))
      .comment("Comment")
      .validator(ValidationFunction("Positive"))

    "build records" >> {
      rec.build must_=== Record(
        Ident("Record"),
        List(GenericDef.of("A", spec("BaseType"))),
        NonEmptyList.of(field("x").ofType(gen("A")).build),
        "Comment".some,
        ValidationFunction("Positive").some
      )
    }

    val en = enum("Test")
      .values(enumValue("A").value("a"))
      .comment("Comment")

    "build enums" >> {
      en.build must_=== Enum(
        Ident("Test"),
        NonEmptyList.of(EnumValue(Ident("A"), "a".some)),
        "Comment".some
      )
    }

    val maybeAdt = adt("Maybe")
      .generic("A".gen)
      .constructors(
        cons("Nothing"),
        cons("Just").generic("A".gen).fields(field("value").ofType("A".gen))
      )

    "build ADTs" >> {
      maybeAdt.build must_=== ADT(
        Ident("Maybe"),
        List(GenericDef.of("A")),
        NonEmptyList.of(
          ADTConstructor(Ident("Nothing"), Nil, Nil),
          ADTConstructor(Ident("Just"), List(GenericDef.of("A")), List(field("value").ofType(gen("A")).build))
        )
      )
    }

    "build packages" >> {
      val p = pkg("org.github.vegansk.test")
        .types(
          simpleNewType, genericNewType, simpleAlias, genericAlias,
          rec, en, maybeAdt
        )

      p.build must_=== Package(
        FQName.of("org.github.vegansk.test"),
        List(
          simpleNewType.build, genericNewType.build,
          simpleAlias.build, genericAlias.build,
          rec.build, en.build, maybeAdt.build
        )
      )
    }

  }

}
