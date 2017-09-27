package sculptor.xsd

import org.specs2._
import scala.xml._

object foldSpec extends mutable.Specification {

  object helper {

    def xsd(n: Node): Node = {
      <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" elementFormDefault="qualified">
        {n}
        </xs:schema>
    }

    def runState[A](s: fold.FoldState)(r: fold.Result[A]): (fold.FoldState, Either[String,A]) =
      r.value.run(s).value

    def run[A](r: fold.Result[A]): (fold.FoldState, Either[String,A]) =
      runState(fold.FoldState(appendPathToError = true, strictMode = true))(r)

    def checkFold[A](initial: A, expected: A)(op: fold.SchemaOp[A])(data: Node) = {
      val res = run {
        op(initial)(xsd(data))
      }

      res._2 must_=== Right(expected)
    }

  }

  import helper._

  "xsd fold" should {

    "detect schema namespace" >> {
      run(
        fold.findSchemaNs {
          <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" />
        }
      )._2 must_=== Right(Some("xs"))
    }

    "implement strict mode" >> {

      val data = <xs:simpleType name="simpleTypeRestriction" />
      runState(
        fold.FoldState(appendPathToError = false, strictMode = true)
      )(
        fold.schema()(())(xsd(data))
      )._2 must_=== Left("Found unprocessed node `simpleType`")

    }

    "detect form default settings" >> {

      val unset = <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" />

      val set =
        <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
      attributeFormDefault="unqualified"
      elementFormDefault="qualified"
        />

      run(
        fold.findAttributeFormDefault(unset)
      )._2 must_=== Right(None)

      run(
        fold.findElementFormDefault(unset)
      )._2 must_=== Right(None)

      run(
        fold.findAttributeFormDefault(set)
      )._2 must_=== Right(Some(fold.Unqualified))

      run(
        fold.findElementFormDefault(set)
      )._2 must_=== Right(Some(fold.Qualified))
    }

    "handle attributes" >> {
      runState(
        fold.FoldState(attributeFormDefault = fold.Unqualified)
      )(
        fold.xsdAttributes {
          <xs:simpleType name="simpleTypeRestriction" otherNs:test="test"/>
        }
      )._2.map(
        _.map {
          case a@Attribute(name, _, _) => (name, a.value.text)
        }
      ) must_=== Right(List(("name", "simpleTypeRestriction")))

      runState(
        fold.FoldState(schemaNs = Some("xs"), attributeFormDefault = fold.Qualified)
      )(
        fold.xsdAttributes {
          <xs:simpleType xs:name="simpleTypeRestriction" test="test"/>
        }
      )._2.map(
        _.map {
          case a@Attribute(name, _, _) => (name, a.value.text)
        }
      ) must_=== Right(List(("name", "simpleTypeRestriction")))
    }

    "fold simple type" >> {

      val simpleType =
        <xs:simpleType name="simpleTypeRestriction">
          <xs:restriction base="xs:integer" />
        </xs:simpleType>

      checkFold(false, true) {
        fold.schema(
          simpleType = fold.simpleTypeOp[Boolean] {
            _ => _ => fold.ok(true)
          }
        )
      } (simpleType)

      checkFold(None, Some("simpleTypeRestriction")) {
        fold.schema(
          simpleType = fold.simpleType(
            restriction = fold.simpleTypeRestrictionOp(fold.nop[Option[String]]),
            name = fold.nameOp {
              _ => n => fold.ok(Some(n))
            }
          )
        )
      } (simpleType)

    }

  }

}
