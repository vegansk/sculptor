package sculptor
package ast

import org.specs2._
import cats.data._
import cats.implicits._
import com.softwaremill.diffx.specs2.DiffMatcher
import com.softwaremill.diffx.generic.DiffDerivation

object astSpec
    extends mutable.Specification
    with DiffMatcher
    with DiffDerivation {

  "sculptor.ast.Newtype" should {

    "describe simple newtype" >> {
      // final case class NewInt(value: Int) extends AnyVal
      val _ =
        Newtype(Ident("NewInt"), Nil, TypeRef.Specialized(FQName(Ident("Int"))))
      true must_=== true
    }

    "describe parameterized newtype" >> {
      // final case class NewList[A](value: List[A]) extends AnyVal
      val genA = TypeRef.Generic(Ident("A"))
      val _ = Newtype(
        Ident("NewList"),
        List(GenericDef(genA)),
        TypeRef.Specialized(FQName(Ident("List")), List(genA))
      )
      true must_=== true
    }
  }

  "sculptor.ast.Alias" should {

    "describe simple alias" >> {
      // type NewInt = Int
      val _ =
        Alias(Ident("NewInt"), Nil, TypeRef.Specialized(FQName(Ident("Int"))))
      true must_=== true
    }

    "describe parameterized alias" >> {
      // type NewList[A] = List[A]
      val genA = TypeRef.Generic(Ident("A"))
      val _ = Alias(
        Ident("NewList"),
        List(GenericDef(genA)),
        TypeRef.Specialized(FQName(Ident("List")), List(genA))
      )
      true must_=== true
    }
  }

  "sculptor.ast.Record" should {
    "describe simple record" >> {
      // final case class Record(a: Int, b: String)
      val fieldA =
        FieldDef(Ident("a"), TypeRef.Specialized(FQName(Ident("Int"))))
      val fieldB =
        FieldDef(Ident("b"), TypeRef.Specialized(FQName(Ident("String"))))
      val _ = Record(Ident("Record"), Nil, NonEmptyList.of(fieldA, fieldB))
      true must_=== true
    }

    "describe parameterized record" >> {
      // final case class Record[A, B](a: A, b: List[B])
      val genA = TypeRef.Generic(Ident("A"))
      val genB = TypeRef.Generic(Ident("B"))
      val fieldA = FieldDef(Ident("a"), genA)
      val fieldB = FieldDef(
        Ident("b"),
        TypeRef.Specialized(FQName(Ident("List")), List(genB))
      )
      val _ = Record(
        Ident("Record"),
        List(GenericDef(genA), GenericDef(genB)),
        NonEmptyList.of(fieldA, fieldB)
      )
      true must_=== true
    }
  }

  "sculptor.ast.Enum" should {
    "describe enumeration" >> {
      /*
       sealed trait Test
       object Test {
         object A extends Test
         object B extends Test

         val asString: Test => String = {
           case A => "theA"
           case B => "theB"
         }

         val fromString: PartialFunction[String, Test] = {
           case "theA" => A
           case "theB" => B
         }
       }
       */
      val enA = EnumValue(Ident("A"), Some("theA"))
      val enB = EnumValue(Ident("B"), Some("theB"))
      val _ = Enum(Ident("Test"), NonEmptyList.of(enA, enB))
      true must_=== true
    }
  }

  "sculptor.ast.ADT" should {
    "describe `Maybe` type" >> {
      /*
       sealed trait Maybe[A]
       case class Just[A] private (value: A) extends Maybe[A]
       object Just {
         def apply[A](value: A): Maybe[A] = new Just[A](value)
       }
       case class Empty[A] private () extends Maybe[A]
       object Empty {
         private val value: Empty[Nothing] = new Empty[Nothing]()
         def apply[A](): Maybe[A] = value.asInstanceOf[Empty[A]]
       }
       */
      val genA = TypeRef.Generic(Ident("A"))
      val just = ADTConstructor(
        Ident("Just"),
        List(GenericDef(genA)),
        List(FieldDef(Ident("value"), genA))
      )
      val empty = ADTConstructor(Ident("Empty"), Nil, Nil)
      val _ = ADT(
        Ident("Maybe"),
        List(GenericDef(genA)),
        NonEmptyList.of(just, empty)
      )
      true must_=== true
    }

    "describe `Either` type" >> {
      /*
       sealed trait Either[E, A]
       case class Left[E, A] private (value: E) extends Either[E, A]
       object Left {
         def apply[E, A](value: E): Either[E, A] = new Left[E, A](value)
       }
       case class Right[E, A] private (value: A) extends Either[E, A]
       object Right {
         def apply[E, A](value: A): Either[E, A] = new Right[E, A](value)
       }
       */
      val genE = TypeRef.Generic(Ident("E"))
      val genA = TypeRef.Generic(Ident("A"))
      val left = ADTConstructor(
        Ident("Left"),
        List(GenericDef(genE), GenericDef(genA)),
        List(FieldDef(Ident("value"), genE))
      )
      val right = ADTConstructor(
        Ident("Right"),
        List(GenericDef(genE), GenericDef(genA)),
        List(FieldDef(Ident("value"), genA))
      )
      val _ = ADT(
        Ident("Either"),
        List(GenericDef(genE), GenericDef(genA)),
        NonEmptyList.of(left, right)
      )
      true must_=== true
    }
  }

  "sculptor.ast.Package" should {
    "sort types" >> {
      val newtype = Newtype(Ident("Z"), Nil, TypeRef.spec("Int"))
      val newtype2 = Newtype(Ident("A"), Nil, TypeRef.spec("Int"))
      val alias = Alias(Ident("Y"), Nil, newtype.ref)
      val record = Record(
        Ident("X"),
        Nil,
        NonEmptyList.of(
          FieldDef(Ident("y"), alias.ref),
          FieldDef(Ident("z"), newtype.ref)
        )
      )
      val pkg = Package(FQName.of("p"), List(record, alias, newtype, newtype2))

      val result = pkg.sortedTypes
      result must_=== List(newtype2, newtype, alias, record).asRight[String]
    }

    "sort according to generic dependencies" >> {
      val alias = Alias(Ident("C"), Nil, TypeRef.spec("Int"))
      val newtype =
        Newtype(Ident("B"), List(GenericDef.of("A")), TypeRef.gen("A"))
      val alias2 = Alias(Ident("A"), Nil, newtype.ref(alias.ref))
      val pkg = Package(FQName.of("p"), List(alias2, newtype, alias))

      val result = pkg.sortedTypes
      result must_=== List(newtype, alias, alias2).asRight[String]
    }

    "sort according to generic dependencies - records" >> {
      val alias = Alias(Ident("B"), Nil, TypeRef.spec("Int"))
      val record = Record(
        Ident("A"),
        Nil,
        NonEmptyList.of(FieldDef(Ident("x"), TypeRef.spec("List", alias.ref)))
      )
      val pkg = Package(FQName.of("p"), List(record, alias))

      val result = pkg.sortedTypes
      result must_=== List(alias, record).asRight[String]
    }

    "support transitive dependencies" >> {
      val enum =
        Enum(Ident("Enum"), NonEmptyList.of(EnumValue(Ident("EnumValue"))))
      val alias = Alias(Ident("Alias"), Nil, enum.ref)
      val newtype = Newtype(Ident("Newtype"), Nil, alias.ref)
      val record = Record(
        Ident("Record"),
        Nil,
        NonEmptyList.of(FieldDef(Ident("field"), newtype.ref))
      )
      val adt = ADT(
        Ident("ADT"),
        Nil,
        NonEmptyList.of(
          ADTConstructor(
            Ident("ADTConstructor"),
            Nil,
            List(FieldDef(Ident("field"), record.ref))
          )
        )
      )
      val pkg = Package(FQName.of("p"), List(adt))
      val result = pkg.sortedTypes
      result must matchTo(
        List(enum, alias, newtype, record, adt).asRight[String]
      )
    }
  }
}
