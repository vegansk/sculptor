package sculptor
package iots
package xsd

import cats.data.{NonEmptyList => NEL}
import cats.implicits._

/** Extracted ASTs for tests */
object testAst {
  import ast._
  import testing.utils._

  def mkRef(t: TypeName): TypeRef = TypeRef(
    t,
    t match {
      case TypeName.std(v) => QName.of(Ident("t"), v)
      case TypeName.custom(QName(v)) => QName(NEL.fromListUnsafe(v.init ++ List(Ident(v.last.value + "Type"))))
    }
  )

  object simpleTypeToEnum {
    lazy val src = parseXsdTypes(
      <xs:simpleType name="test_et">
        <xs:restriction base="xs:string">
          <xs:enumeration value="01"/>
          <xs:enumeration value="02"/>
        </xs:restriction>
      </xs:simpleType>).head

    lazy val dst =
      EnumDecl(
      Ident("TestEt"),
      Ident("TestEtType"),
      true,
      NEL.of(
        EnumMemberDecl(Ident("V_01"), "01"),
        EnumMemberDecl(Ident("V_02"), "02")
      )
    )
  }

  object complexTypeToTypeDecl {
    lazy val src = parseXsdTypes(
      <xs:complexType name="id_t">
        <xs:sequence>
          <xs:element name="id" type="xs:string"/>
          <xs:element name="org" type="xs:string" minOccurs="0"/>
        </xs:sequence>
      </xs:complexType>).head

    lazy val dst =
      ComplexTypeDecl(
        Ident("IdT"),
        Ident("IdTType"),
        None,
        true,
         NEL.of(
          FieldDecl(
            Ident("id"),
            mkRef(TypeName.std(Ident("string"))),
            FieldConstraint.Required, false),
          FieldDecl(
            Ident("org"),
            mkRef(TypeName.std(Ident("string"))),
            FieldConstraint.Optional, false))
      )
  }

  object complexTypeAnonymousSeq {
    lazy val src = parseXsdTypes(
      <xs:complexType name="type_t">
        <xs:sequence>
          <xs:sequence minOccurs="0">
            <xs:element name="value1" type="xs:string" minOccurs="0"/>
          </xs:sequence>
          <xs:element name="value2" type="xs:string"/>
          <xs:sequence>
            <xs:element name="value3" type="xs:string"/>
          </xs:sequence>
          <xs:choice minOccurs="0">
            <xs:element name="value4" type="xs:string"/>
          </xs:choice>
          <xs:choice>
            <xs:element name="value5" type="xs:string" minOccurs="0"/>
          </xs:choice>
        </xs:sequence>
      </xs:complexType>).head

    lazy val outerType =
      ComplexTypeDecl(
        Ident("TypeT"),
        Ident("TypeTType"),
        None,
        true,
        NEL.of(
          FieldDecl(
            Ident("value1"),
            mkRef(TypeName.std(Ident("string"))),
            FieldConstraint.Optional, false),
          FieldDecl(
            Ident("value2"),
            mkRef(TypeName.std(Ident("string"))),
            FieldConstraint.Required, false),
          FieldDecl(
            Ident("value3"),
            mkRef(TypeName.std(Ident("string"))),
            FieldConstraint.Required, false),
          FieldDecl(
            Ident("value4"),
            mkRef(TypeName.std(Ident("string"))),
            FieldConstraint.Optional, false),
          FieldDecl(
            Ident("value5"),
            mkRef(TypeName.std(Ident("string"))),
            FieldConstraint.Optional, false))
      )
    lazy val dst = List(outerType)
  }

  object elementToTypeDecl {
    lazy val src = parseXsdTypes(
      <xs:element name="TEST_ELEM">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="x" type="xs:int"/>
            <xs:element name="y" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    ).head

    lazy val dst =
      ComplexTypeDecl(
        Ident("TestElem"),
        Ident("TestElemType"),
        None,
        true,
        NEL.of(
          FieldDecl(
            Ident("x"),
            mkRef(TypeName.std(Ident("number"))),
            FieldConstraint.Required, false),
          FieldDecl(
            Ident("y"),
            mkRef(TypeName.std(Ident("string"))),
            FieldConstraint.Required, false))
      )
  }

  object complexTypeWithChoiceToTypeDecl {
    lazy val src = parseXsdTypes(
      <xs:complexType name="test">
        <xs:choice>
          <xs:element name="num" type="xs:int"/>
          <xs:element name="str" type="xs:string" minOccurs="0"/>
        </xs:choice>
      </xs:complexType>).head

    lazy val dst =
      ComplexTypeDecl(
        Ident("Test"),
        Ident("TestType"),
        None,
        true,
        NEL.of(
          FieldDecl(
            Ident("num"),
            mkRef(TypeName.std(Ident("number"))),
            FieldConstraint.Optional, false),
          FieldDecl(
            Ident("str"),
            mkRef(TypeName.std(Ident("string"))),
            FieldConstraint.Optional, false))
      )
  }

  object complexTypeInheritance {
    lazy val src = parseXsdTypes(
      <xs:complexType name="child_t">
        <xs:complexContent>
          <xs:extension base="base_t">
            <xs:sequence>
              <xs:element name="value" type="xs:string"/>
            </xs:sequence>
          </xs:extension>
        </xs:complexContent>
      </xs:complexType>
    ).head

    lazy val dst =
      ComplexTypeDecl(
        Ident("ChildT"),
        Ident("ChildTType"),
        TypeRef(
          TypeName.custom(QName.of(Ident("BaseT"))),
          QName.of(Ident("BaseTType"))
        ).some,
        true,
        NEL.of(
          FieldDecl(
            Ident("value"),
            mkRef(TypeName.std(Ident("string"))),
            FieldConstraint.Required, false))
      )
  }

  object simpleTypeRestriction {
    lazy val src = parseXsdTypes(
      <xs:simpleType name="decimal_t">
        <xs:restriction base="xs:decimal"/>
      </xs:simpleType>
    ).head

    lazy val dst =
      NewtypeDecl(
        Ident("DecimalT"),
        Ident("DecimalTType"),
        TypeRef(
          TypeName.std(Ident("number")),
          QName.of(Ident("t"), Ident("number"))
        ),
        true
      )
  }

  object anonComplexType {
    lazy val src = parseXsdTypes(
      <xs:element name="TEST">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="value">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="id" type="xs:int"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    ).head

    val anonType =
      ComplexTypeDecl(
        Ident("TestValue"),
        Ident("TestValueType"),
        None,
        true,
        NEL.of(
          FieldDecl(
            Ident("id"),
            mkRef(TypeName.std(Ident("number"))),
            FieldConstraint.Required, false))
      )

    val elType =
      ComplexTypeDecl(
        Ident("Test"),
        Ident("TestType"),
        None,
        true,
        NEL.of(
          FieldDecl(
            Ident("value"),
            mkRef(TypeName.custom(QName.of(Ident("TestValue")))),
            FieldConstraint.Required, false))
      )

    lazy val dst = List(anonType, elType)
  }

  object complexTypeAttributes {
    lazy val src = parseXsdTypes(
      <xs:complexType name="test">
        <xs:sequence>
          <xs:element name="value" type="xs:string"/>
        </xs:sequence>
        <xs:attribute name="id" type="xs:int"/>
      </xs:complexType>
    ).head

    lazy val dst =
      ComplexTypeDecl(
        Ident("Test"),
        Ident("TestType"),
        None,
        true,
        NEL.of(
          FieldDecl(
            Ident("value"),
            mkRef(TypeName.std(Ident("string"))),
            FieldConstraint.Required, false),
          FieldDecl(
            Ident("id"),
            mkRef(TypeName.std(Ident("number"))),
            FieldConstraint.Required, false))
      )
  }

  object complexTypeFieldsConstraints {
    lazy val src = parseXsdTypes(
      <xs:complexType name="test">
        <xs:sequence>
          <xs:element name="optional" type="xs:string" minOccurs="0" maxOccurs="1"/>
          <xs:element name="nullable" type="xs:string" nillable="true"/>
          <xs:element name="array" type="xs:string" maxOccurs="unbounded"/>
          <xs:element name="optional_nullable" type="xs:string" minOccurs="0" nillable="true"/>
          <xs:element name="optional_array" type="xs:string" minOccurs="0" maxOccurs="2" nillable="true"/>
        </xs:sequence>
      </xs:complexType>
    ).head

    lazy val dst =
      ComplexTypeDecl(
        Ident("Test"),
        Ident("TestType"),
        None,
        true,
        NEL.of(
          FieldDecl(
            Ident("optional"),
            mkRef(TypeName.std(Ident("string"))),
            FieldConstraint.Optional, false),
          FieldDecl(
            Ident("nullable"),
            mkRef(TypeName.std(Ident("string"))),
            FieldConstraint.Nullable, false),
          FieldDecl(
            Ident("array"),
            mkRef(TypeName.std(Ident("string"))),
            FieldConstraint.Required, true),
          FieldDecl(
            Ident("optionalNullable"),
            mkRef(TypeName.std(Ident("string"))),
            FieldConstraint.OptionalNullable, false),
          FieldDecl(
            Ident("optionalArray"),
            mkRef(TypeName.std(Ident("string"))),
            FieldConstraint.Optional, true))
      )
  }
}
