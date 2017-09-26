package sculptor.xsd

import org.specs2._
import scala.xml.XML

object foldSpec extends mutable.Specification {

  "xsd fold" should {

    "fold fes-1.0 schema" >> {

      val resource =
        getClass.getClassLoader.getResourceAsStream("xsd/fes-1.0.xsd")

      type Result = Unit

      lazy val annotation: fold.AnnotationOp[Result] = fold.annotation()

      lazy val attribute: fold.AttributeOp[Result] = fold.attribute()

      lazy val list: fold.ListOp[Result] = fold.list(
        annotation = annotation,
        simpleType = simpleType
      )

      lazy val restriction: fold.SimpleTypeRestrictionOp[Result] = fold.simpleTypeRestriction(
        simpleType = simpleType
      )

      lazy val union: fold.UnionOp[Result] = fold.union(
        annotation = annotation,
        simpleType = simpleType
      )

      lazy val simpleType: fold.SimpleTypeOp[Result] = fold.simpleType(
        annotation = annotation,
        list = list,
        restriction = restriction,
        union = union
      )

      lazy val complexType: fold.ComplexTypeOp[Result] = fold.complexType(
        annotation = annotation,
        attribute = attribute
      )

      val xsd = XML.load(resource)

      val f = fold.schema(
        annotation = annotation,
        simpleType = simpleType,
        complexType = complexType,
        attribute = attribute
      )(())(xsd)

      val res = f.value.run(fold.FoldState()).value
      println((res._1.schemaNs, res._2))
      res._2.isRight must_== true

    }

  }

}
