package sculptor.xsd

import org.specs2._
import scala.xml.XML


object foldSpec extends mutable.Specification {

  "xsd fold" should {

    "fold fes-1.0 schema" >> {

      val resource =
        getClass.getClassLoader.getResourceAsStream("xsd/fes-1.0.xsd")

      val nop: fold.Op[Unit] = a => _ => fold.ok(a)

      val f = fold.schema(
        annotation = nop,
        simpleType = nop,
        complexType = nop,
        element = nop,
      )(())(XML.load(resource))

      val res = f.value.run(fold.FoldState()).value
      println((res._1.schemaNs, res._2))
      res._2.isRight must_== true

    }

  }

}
