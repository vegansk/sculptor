package sculptor
package scala
package xsd

import org.specs2._
import cats._
import cats.implicits._

object transformSpec extends mutable.Specification
    with ScalaCheck
    with testing.CatsEqMatcher {

  "scala module" should {

    import xsd._
    import ast._
    import sculptor.xsd.{ast => x}
    import testAst._

    val config = Config(
      List(
        ImportDecl("java.time.Instant")
      ),
      "xs".some,
      List(
        ExternatType(
          x.QName("t1", None),
          QName.of(Ident("fake")),
          QName.of(Ident("fake"))
        )
      )
    )

    def schema(t: x.Type[Id]*): x.Schema[Id] = x.Schema[Id](
      None,
      t.toList
    )

    def transformSchema(t: x.Type[Id]*): ModuleDecl = {
      val s = schema(t:_*)
      transform(s).value(config) match {
        case Right(v) => v
        case Left(err) => sys.error(s"Transformation error: $err")
      }
    }

    "generate imports" >> {
      val cfg = config.copy(
        imports = List(
          ImportDecl("a"),
          ImportDecl("b")
        )
      )

      val m = transform(schema()).value(cfg).right.get

      m.imports.get.value must_=== cfg.imports
    }

    "transform simple type to enum declaration" >> {
      val ast = transformSchema(simpleTypeToEnum.src)
      ast.types.get.value.head must_=== simpleTypeToEnum.dst
    }

    "transform complex type to type declaration" >> {
      val m = transformSchema(complexTypeToTypeDecl.src)
      m.types.get.value.head must_=== complexTypeToTypeDecl.dst
    }

    "transform complex type with anonymous sequences" >> {
      val m = transformSchema(complexTypeAnonymousSeq.src)
      m.types.get.value.some must_=== complexTypeAnonymousSeq.dst.toNel
    }

    "transform element to type declaration" >> {
      val m = transformSchema(elementToTypeDecl.src)
      m.types.get.value.head must_=== elementToTypeDecl.dst
    }

    "transform complex type with choice to type declaration" >> {
      val m = transformSchema(complexTypeWithChoiceToTypeDecl.src)
      m.types.get.value.head must_=== complexTypeWithChoiceToTypeDecl.dst
    }

    "transform complex type with parent to type declaration" >> {
      val m = transformSchema(complexTypeInheritance.src)
      m.types.get.value.head must_=== complexTypeInheritance.dst
    }

    "transform simple type to newtype declaration" >> {
      val m = transformSchema(simpleTypeRestriction.src)
      m.types.get.value.head must_=== simpleTypeRestriction.dst
    }

    "transform element with anonymous complex type" >> {
      val m = transformSchema(anonComplexType.src)
      m.types.get.value.some must_=== anonComplexType.dst.toNel
    }

    "transform complex type with attributes" >> {
      val m = transformSchema(complexTypeAttributes.src)
      m.types.get.value.head must_=== complexTypeAttributes.dst
    }

    "transform complex type fields constraints" >> {
      val m = transformSchema(complexTypeFieldsConstraints.src)
      m.types.get.value.head must_=== complexTypeFieldsConstraints.dst
    }

    "detect cyclic dependencies" >> {
      val _ = transformSchema(cyclicDependencies.src:_*)
      // This will never be reached on error
      true must_=== true
    }

    "transform complex type extension to alias" >> {
      val m = transformSchema(complexTypeAlias.src)
      m.types.get.value.head must_=== complexTypeAlias.dst
    }

  }

}
