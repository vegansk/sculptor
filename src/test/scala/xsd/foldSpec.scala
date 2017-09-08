package sculptor.xsd

import org.specs2._
import scala.xml.XML


object foldSpec extends mutable.Specification {

  "xsd fold" should {

    "fold fes-1.0 schema" >> {

      val resource =
        getClass.getClassLoader.getResourceAsStream("xsd/fes-1.0.xsd")

      type Result = Unit

      val annotation = fold.annotation[Result](fold.nop, fold.nop)

      val attribute = fold.attribute[Result](fold.nop, fold.nop)

      val list = fold.list[Result](
        annotation = annotation,
        //TODO: Fight specs2 recursion crash
        // simpleType = simpleType
        simpleType = fold.nop
      )

      val restriction = fold.restriction[Result](
        annotation = fold.nop,
        //TODO: Fight specs2 recursion crash
        // simpleType = simpleType
        simpleType = fold.nop,
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

      val union = fold.union[Result](
        annotation = annotation,
        //TODO: Fight specs2 recursion crash
        // simpleType = simpleType
        simpleType = fold.nop
      )

      val simpleType = fold.simpleType[Result](
        annotation = annotation,
        list = list,
        restriction = restriction,
        union = union
      )

      val complexType = fold.complexType(
        annotation = annotation,
        simpleContent = fold.nop,
        complexContent = fold.nop,
        group = fold.nop,
        all = fold.nop,
        choice = fold.nop,
        sequence = fold.nop,
        attribute = attribute
      )

      val f = fold.schema(
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
