package sculptor.xsd

import org.specs2._
import cats.implicits._


object integrationSpec extends mutable.Specification {

  import utils._
  import ast._

  "xsd tools" should {

    "produce ast from fes-1.0 schema" >> {
      import scala.xml._
      val result = run[Schema[Option]] {
        parser[Option].parse {
          XML.load(
            getClass.getClassLoader.getResourceAsStream("xsd/fes-1.0.xsd")
          )
        }
      }

      result._2 must beRight(
        (xsdO: Schema[Option]) => {
          val xsd = Schema.build(xsdO)

          xsd.toEither must beRight
        }
      )
    }

    "produce ast from ca_iso20022_v2 schema" >> {
      import scala.xml._
      val result = run[Schema[Option]] {
        parser[Option].parse {
          XML.load(
            getClass.getClassLoader.getResourceAsStream("xsd/ca_iso20022_v2.xsd")
          )
        }
      }

      result._2 must beRight(
        (xsdO: Schema[Option]) => {
          val xsd = Schema.build(xsdO)

          xsd.toEither must beRight
        }
      )
    }

  }

}
