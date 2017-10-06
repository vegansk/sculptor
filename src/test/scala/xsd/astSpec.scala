package sculptor.xsd

import org.specs2._
import cats.{ Id }
import cats.data._
import cats.implicits._

object astSpec extends mutable.Specification {

  import ast._

  "ast" should {

    import Validated._

    val goodAnnotation = Annotation[Option](Some(List("test")))

    "build Annotation" >> {

      val badAnnotation = Annotation[Option](None)
      Annotation.build(badAnnotation).isInvalid must_=== true

      Annotation.build(goodAnnotation) must_=== valid(
        Annotation[Id](List("test"))
      )
    }

    val goodEnumeration = Enumeration[Option](
      Some("VALUE"),
      Option(Option(goodAnnotation))
    )

    "build Enumeration" >> {

      val badEnumeration = Enumeration[Option](None, None)
      Enumeration.build(badEnumeration).isInvalid must_=== true

      Enumeration.build(goodEnumeration) must_=== valid(
        Enumeration[Id](
          "VALUE",
          Option(Annotation.build(goodAnnotation).toOption.get)
        )
      )
    }

    val goodSimpleTypeRestriction = SimpleTypeRestriction[Option](
      base = Option("base"),
      pattern = Option("pattern".some),
      minLength = Option("minLength".some),
      maxLength = Option("maxLength".some),
      enumeration = Option(List(goodEnumeration, goodEnumeration))
    )

    "build SimpleTypeRestriction" >> {

      val badSimpleTypeRestriction = SimpleTypeRestriction.empty[Option]
      SimpleTypeRestriction.build(badSimpleTypeRestriction).isInvalid must_=== true

      SimpleTypeRestriction.build(goodSimpleTypeRestriction) must_=== valid(
        SimpleTypeRestriction[Id](
          base = "base",
          pattern = "pattern".some,
          minLength = "minLength".some,
          maxLength = "maxLength".some,
          enumeration = List(
            Enumeration.build(goodEnumeration).toOption.get,
            Enumeration.build(goodEnumeration).toOption.get
          )
        )
      )
    }

  }

}
