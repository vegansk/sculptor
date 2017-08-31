package sculptor.xsd

import org.specs2._
import sculptor._
import scala.xml.XML

object parserSpec extends mutable.Specification {

  "xsd parser" should {

    "parse xsd" >> {

      val xsd =
        getClass.getClassLoader.getResourceAsStream("xsd/parserSpec_01.xsd")

      parser(XML.load(xsd)) must_== Right(
        types.Module(
          None,
          List(
            types.RStr(
              Some("restricted_string_t"),
              Some(1),
              Some(10),
              List("[a-z]+", "[abc]+")
            ),
            types.ROrdinal(Some("restricted_integer_t"), types.Int),
            types.Record(
              Some("record_t"),
              Map("str" -> types.Str, "int" -> types.Int)
            )
          )
        )
      )

    }

  }

}
