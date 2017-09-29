package sculptor.xsd

import org.specs2._
import scala.xml._
import cats._, cats.implicits._

object foldSpec extends mutable.Specification {

  import utils._

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
        fold.schema()(xsd(data))(())
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

    "handle children" >> {
      val data =
        <xs:simpleType name="simpleTypeRestriction">
          <xs:restriction base="xs:integer" />
        </xs:simpleType>
      runState(
        fold.FoldState(
          schemaNs = Some("xs"),
          elementFormDefault = fold.Qualified,
          strictMode = true
        )
      )(
        fold.xsdChild(xsd(data))
      )._2.map(_.map(_.label)) must_=== Right(List("simpleType"))
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
      } (xsd(simpleType))

      checkFold(None, Some("simpleTypeRestriction")) {
        fold.schema(
          simpleType = fold.simpleType(
            restriction = fold.simpleTypeRestrictionOp(fold.nop[Option[String]]),
            name = fold.nameOp {
              n => _ => fold.ok(Some(n))
            }
          )
        )
      } (xsd(simpleType))

    }

    "fold tree structures" >> {
      val data =
        <xs:simpleType name="simpleTypeRestriction">
          <xs:restriction>
              <xs:simpleType>
                  <xs:restriction base="xs:string">
          {
                      // <xs:enumeration value="A"/>
                      // <xs:enumeration value="B"/>
          }
                  </xs:restriction>
              </xs:simpleType>
          </xs:restriction>
        </xs:simpleType>

      import testAst._

      def simpleType[A[?[_]] <: AST[?], F[_]: MonoidK: Applicative](f: SimpleType[F] => A[F] => A[F]): fold.SimpleTypeOp[A[F]] = {
        fold.simpleTypeOp[A[F]](
          node => ast => for {
            st <- fold.simpleType[SimpleType[F]](
              name = simpleTypeName[F],
              restriction = simpleTypeRestriction[F]
            )(node)(SimpleType[F]())
          } yield f(st)(ast)
        )
      }


      def simpleTypeName[F[_]: Applicative] = fold.nameOp[SimpleType[F]](
        name => ast => fold.ok(ast.copy[F](name = Applicative[F].pure(Some(name))))
      )

      def restrictionBase[F[_]: Applicative] = fold.baseOp[Restriction[F]](
        name => ast => fold.ok(ast.copy[F](base = Applicative[F].pure(name)))
      )

      def restrictionSimpleType[F[_]: MonoidK: Applicative] = simpleType[Restriction, F] {
        st => r => r.copy[F](simpleType = Applicative[F].pure(Some(st)))
      }

      def simpleTypeRestriction[F[_]: MonoidK: Applicative] = fold.simpleTypeRestrictionOp[SimpleType[F]](
        node => ast => for {
            str <- fold.simpleTypeRestriction(
              base = restrictionBase[F],
              simpleType = restrictionSimpleType[F]
            )(node)(Restriction[F]())
          } yield ast.copy[F](restriction = Applicative[F].pure(Some(str)))
      )

      def moduleSimpleType[F[_]: Alternative] = simpleType[Module, F] {
        st => m => m.copy[F](
          types = Alternative[F].combineK(m.types, Alternative[F].pure(List(st)))
        )
      }

      checkFold[Module[Option]](
        Module[Option](types = None),
        Module[Option](
          types = Some(
            List(
              SimpleType[Option](
                name = Some(Some("simpleTypeRestriction")),
                restriction = Some(
                  Some(
                    Restriction[Option](
                      base = None,
                      simpleType = Some(
                        Some(
                          SimpleType[Option](
                            name = None,
                            restriction = Some(
                              Some(
                                Restriction[Option](
                                  base = Some("xs:string"),
                                  simpleType = None
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )(
        fold.schema(
          simpleType = moduleSimpleType[Option]
        )
      )(xsd(data))

    }
  }
}
