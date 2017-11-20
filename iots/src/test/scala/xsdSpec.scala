package sculptor
package iots

import org.specs2._

object xsdSpec extends mutable.Specification {

  "iots.xsd.fold" should {

    "fold simple types" >> {

      import scala.xml._

      val result = generator.fromNode(
        XML.load(
          getClass.getClassLoader.getResourceAsStream("xsd/simpleTypes.xsd")
        )
      )

      println(result.map(_.render(80)).getOrElse(""))
      result.isValid must_== true
    }

  }

}
