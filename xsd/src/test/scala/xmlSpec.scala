package sculptor.xsd

import org.specs2._
import scala.xml._

object xmlSpec extends mutable.Specification {

  val doc = XML.loadString {
    """
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" elementFormDefault="qualified">
    <xs:complexType name="Record">
        <xs:sequence>
            <xs:element name="theString" type="xs:string"/>
            <xs:element name="theInteger" type="xs:integer"/>
        </xs:sequence>
    </xs:complexType>
</xs:schema>
"""
  }

  val complexType = (doc \\ "complexType").head

  val sequence = (doc \\ "sequence").head

  "xml utils" should {

    "return child by it's name" >> {

      xml.getByName("sequence")(complexType).isRight must_== true
      xml.getByName("sequence", Some("xs"))(complexType).isRight must_== true

    }

    "return list of childs" >> {

      xml.findByName("element")(sequence).size must_== 2
      xml.findByName("element", Some("xs"))(sequence).size must_== 2

    }

  }

}
