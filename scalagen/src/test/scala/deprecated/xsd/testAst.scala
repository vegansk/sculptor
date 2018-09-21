package sculptor.scalagen.deprecated
package xsd

import cats.data.{NonEmptyList => NEL}
import cats.implicits._

/** Extracted ASTs for tests */
object testAst {
  import ast._
  import sculptor.scalagen.testing.utils._

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
      TypeRef.definedFrom("TestEt"),
      NEL.of(
        EnumMemberDecl(Ident("V_01"), "01", None),
        EnumMemberDecl(Ident("V_02"), "02", None)
      ),
      None
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
        TypeRef.definedFrom("IdT"),
        None,
        NEL.of(
          FieldDecl(
            Ident("id"),
            "id",
            TypeRef.std(Ident("String")),
            FieldConstraint.Required, false, false, None),
          FieldDecl(
            Ident("org"),
            "org",
            TypeRef.std(Ident("String")),
            FieldConstraint.Optional, false, false, None)),
        None
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
        TypeRef.definedFrom("TypeT"),
        None,
        NEL.of(
          FieldDecl(
            Ident("value1"),
            "value1",
            TypeRef.std(Ident("String")),
            FieldConstraint.Optional, false, false, None),
          FieldDecl(
            Ident("value2"),
            "value2",
            TypeRef.std(Ident("String")),
            FieldConstraint.Required, false, false, None),
          FieldDecl(
            Ident("value3"),
            "value3",
            TypeRef.std(Ident("String")),
            FieldConstraint.Required, false, false, None),
          FieldDecl(
            Ident("value4"),
            "value4",
            TypeRef.std(Ident("String")),
            FieldConstraint.Optional, false, false, None),
          FieldDecl(
            Ident("value5"),
            "value5",
            TypeRef.std(Ident("String")),
            FieldConstraint.Optional, false, false, None)),
        None
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
        TypeRef.definedFrom("TestElem"),
        None,
        NEL.of(
          FieldDecl(
            Ident("x"),
            "x",
            TypeRef.std(Ident("Int")),
            FieldConstraint.Required, false, false, None),
          FieldDecl(
            Ident("y"),
            "y",
            TypeRef.std(Ident("String")),
            FieldConstraint.Required, false, false, None)),
        None
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
        TypeRef.definedFrom("Test"),
        None,
        NEL.of(
          FieldDecl(
            Ident("num"),
            "num",
            TypeRef.std(Ident("Int")),
            FieldConstraint.Optional, false, false, None),
          FieldDecl(
            Ident("str"),
            "str",
            TypeRef.std(Ident("String")),
            FieldConstraint.Optional, false, false, None)),
        None
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
        TypeRef.definedFrom("ChildT"),
        TypeRef.definedFrom("BaseT").some,
        NEL.of(
          FieldDecl(
            Ident("value"),
            "value",
            TypeRef.std(Ident("String")),
            FieldConstraint.Required, false, false, None)),
        None
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
        TypeRef.definedFrom("DecimalT"),
        TypeRef.std(Ident("Double")),
        None
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
        TypeRef.definedFrom("TestValue"),
        None,
        NEL.of(
          FieldDecl(
            Ident("id"),
            "id",
            TypeRef.std(Ident("Int")),
            FieldConstraint.Required, false, false, None)),
        None
      )

    val elType =
      ComplexTypeDecl(
        TypeRef.definedFrom("Test"),
        None,
        NEL.of(
          FieldDecl(
            Ident("value"),
            "value",
            TypeRef.defined(Ident("TestValue")),
            FieldConstraint.Required, false, false, None)),
        None
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
        TypeRef.definedFrom("Test"),
        None,
        NEL.of(
          FieldDecl(
            Ident("value"),
            "value",
            TypeRef.std(Ident("String")),
            FieldConstraint.Required, false, false, None),
          FieldDecl(
            Ident("id"),
            "id",
            TypeRef.std(Ident("Int")),
            FieldConstraint.Required, true, false, None)),
        None
      )
  }

  object complexTypeSimpleContent {
    lazy val src = parseXsdTypes(
      <xs:complexType name="test">
        <xs:simpleContent>
          <xs:extension base="base">
            <xs:attribute name="id" type="xs:int" use="required" />
          </xs:extension>
        </xs:simpleContent>
      </xs:complexType>
    ).head

    lazy val dst =
      SimpleTypeExtensionDecl(
        TypeRef.definedFrom("test"),
        TypeRef.definedFrom("base"),
        NEL.of(
          FieldDecl(
            Ident("baseContent"),
            "",
            TypeRef.definedFrom("base"),
            FieldConstraint.Required, false, false, None),
          FieldDecl(
            Ident("id"),
            "id",
            TypeRef.std(Ident("int")),
            FieldConstraint.Required, true, false, None)),
        None
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
        TypeRef.definedFrom("Test"),
        None,
        NEL.of(
          FieldDecl(
            Ident("optional"),
            "optional",
            TypeRef.std(Ident("String")),
            FieldConstraint.Optional, false, false, None),
          FieldDecl(
            Ident("nullable"),
            "nullable",
            TypeRef.std(Ident("String")),
            FieldConstraint.Nullable, false, false, None),
          FieldDecl(
            Ident("array"),
            "array",
            TypeRef.std(Ident("String")),
            FieldConstraint.Required, false, true, None),
          FieldDecl(
            Ident("optionalNullable"),
            "optional_nullable",
            TypeRef.std(Ident("String")),
            FieldConstraint.OptionalNullable, false, false, None),
          FieldDecl(
            Ident("optionalArray"),
            "optional_array",
            TypeRef.std(Ident("String")),
            FieldConstraint.Optional, false, true, None)),
        None
      )
  }

  object cyclicDependencies {
    lazy val src = parseXsdTypes(
      Seq(
        <xs:complexType name="t1">
          <xs:sequence>
            <xs:element name="t2" type="t2"/>
          </xs:sequence>
        </xs:complexType>,
        <xs:complexType name="t2">
          <xs:sequence>
            <xs:element name="t3" type="t3"/>
          </xs:sequence>
        </xs:complexType>,
        <xs:complexType name="t3">
          <xs:sequence>
            <xs:element name="t1" type="t1"/>
          </xs:sequence>
        </xs:complexType>
      )
    )
  }

  object complexTypeAlias {
    lazy val src = parseXsdTypes(
      <xs:complexType name="alias_t">
        <xs:complexContent>
          <xs:extension base="type_t"/>
        </xs:complexContent>
      </xs:complexType>
    ).head

    lazy val dst =
      NewtypeDecl(
        TypeRef.definedFrom("AliasT"),
        TypeRef.definedFrom("TypeT"),
        None
      )
  }
}
