package sculptor.xsd

import org.specs2._
import scala.xml.XML

object foldSpec extends mutable.Specification {

  "xsd fold" should {

    "fold fes-1.0 schema" >> {

      val resource =
        getClass.getClassLoader.getResourceAsStream("xsd/fes-1.0.xsd")

      type Result = Unit

      lazy val annotation: fold.Op[Result] = fold.annotation[Result]()

      lazy val attribute: fold.Op[Result] = fold.attribute[Result]()

      lazy val list: fold.Op[Result] = fold.list[Result](
        annotation = annotation,
        simpleType = simpleType
      )

      lazy val restriction: fold.Op[Result] = fold.simpleTypeRestriction[Result](
        annotation = fold.nop,
        simpleType = simpleType,
        minExclusive = fold.nop,
        minInclusive = fold.nop,
        maxExclusive = fold.nop,
        maxInclusive = fold.nop,
        totalDigits = fold.nop,
        fractionDigits = fold.nop,
        length = fold.nop,
        minLength = fold.nop,
        maxLength = fold.nop,
        enumeration = fold.nop,
        whiteSpace = fold.nop,
        pattern = fold.nop
      )

      lazy val union: fold.Op[Result] = fold.union[Result](
        annotation = annotation,
        simpleType = simpleType
      )

      lazy val simpleType: fold.Op[Result] = fold.simpleType[Result](
        annotation = annotation,
        list = list,
        restriction = restriction,
        union = union
      )

      lazy val complexType: fold.Op[Result] = fold.complexType(
        annotation = annotation,
        simpleContent = fold.nop,
        complexContent = fold.nop,
        group = fold.nop,
        all = fold.nop,
        choice = fold.nop,
        sequence = fold.nop,
        attribute = attribute
      )

      lazy val f = fold.schema(
        annotation = annotation,
        simpleType = simpleType,
        complexType = complexType,
        element = fold.nop,
        attribute = attribute
      )(())(XML.load(resource))

      val res = f.value.run(fold.FoldState()).value
      println((res._1.schemaNs, res._2))
      res._2.isRight must_== true

    }

  }

}
