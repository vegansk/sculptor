package sculptor.xsd

import org.specs2._
import cats.implicits._

object parserSpec extends mutable.Specification {

  import utils._
  import ast._

  "xsd parser" should {

    "parse empty schema" >> {
      checkParser(
        Schema.empty[Option]
      ) {
        parser[Option].parse {
          <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" elementFormDefault="qualified">
          </xs:schema>
        }
      }
    }

    "parse schema annotation" >> {

      checkParser(
        Schema[Option](
          Some(Some(Annotation(Some(List("test"))))),
          None
        )
      ) {
        parser[Option].parse {
          <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" elementFormDefault="qualified">
            <xs:annotation>
              <xs:documentation>test</xs:documentation>
            </xs:annotation>
          </xs:schema>
        }
      }
    }

    "parse simpleType with pattern set" >> {
      val result = run {
        parser[Option].parse {
          <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" elementFormDefault="qualified">
            <xs:simpleType name="test_t">
            <xs:annotation>
            <xs:documentation>test</xs:documentation>
            </xs:annotation>
            <xs:restriction base="xs:string">
            <xs:whiteSpace value="collapse"/>
            <xs:pattern value="[0-9]+"/>
            </xs:restriction>
            </xs:simpleType>
            </xs:schema>
        }
      }

      result._2.isRight must_=== true
    }

    "parse simpleType with enumerations" >> {
      val result = run {
        parser[Option].parse {
          <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" elementFormDefault="qualified">
            <xs:simpleType name="test_t">
                <xs:annotation>
                    <xs:documentation>test</xs:documentation>
                </xs:annotation>
                <xs:restriction base="xs:string">
                  <xs:enumeration value="V1">
                    <xs:annotation>
                      <xs:documentation>V1 value</xs:documentation>
                    </xs:annotation>
                  </xs:enumeration>
                  <xs:enumeration value="V2">
                    <xs:annotation>
                      <xs:documentation>V2 value</xs:documentation>
                    </xs:annotation>
                  </xs:enumeration>
                </xs:restriction>
            </xs:simpleType>
          </xs:schema>
        }
      }

      result._2.isRight must_=== true
    }

    "parse fes-1.0 schema" >> {
      import scala.xml._
      val result = run {
        parser[Option].parse {
          XML.load(
            getClass.getClassLoader.getResourceAsStream("xsd/fes-1.0.xsd")
          )
        }
      }

      println(result._2)
      // It'll be true when the parser will be finished :-)
      result._2.isRight must_=== false
    }

  }

}
