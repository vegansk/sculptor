package sculptor.xsd

import org.specs2._
import sculptor._
import types._
import scala.xml.XML

object parserSpec extends mutable.Specification {

  "xsd parser" should {

    "parse xsd" >> {

      val xsd =
        getClass.getClassLoader.getResourceAsStream("xsd/parserSpec_01.xsd")

      parser(XML.load(xsd)) must_== Right(
        ModuleF(
          None,
          List(
            TypeT(
              RestrictedStringF(
                Some("restricted_string_t"),
                TypeT(StringF()),
                Some(1),
                Some(10),
                List("[a-z]+", "[abc]+")
              )
            ),
            TypeT(
              RestrictedNumberF(Some("restricted_integer_t"), TypeT(IntF()))
            ),
            TypeT(
              RecordF(
                Some("record_t"),
                List("str" -> TypeT(StringF()), "int" -> TypeT(IntF()))
              )
            )
          )
        )
      )

    }

  }

}
