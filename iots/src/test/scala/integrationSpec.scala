package sculptor
package iots

import org.specs2._
import scala.xml._
import cats._
import cats.implicits._

object integrationSpec extends mutable.Specification {
  import testing.utils._
  import sculptor.iots.xsd._
  import sculptor.xsd.{ast => x}
  import ast._

  val config = Config(
    ImportDecl(Ident("t"), "io-ts"),
    ImportDecl(Ident("T"), "core/types"),
    Nil,
    "xsd".some
  )

  def transformSchema(xsd: x.Schema[Id]): ModuleDecl = {
    transform(xsd).value(config) match {
      case Right(v) => v
      case Left(err) => sys.error(s"Transformation error: $err")
    }
  }

  "iots module" should {
    "produce iots sources from fes-1.0.xsd" >> {
      val xsd = parseXsd(
        XML.load(
          getClass.getClassLoader.getResourceAsStream("xsd/fes-1.0.xsd")
        )
      )

      transformSchema(xsd)

      true must_=== true
    }
  }

}
