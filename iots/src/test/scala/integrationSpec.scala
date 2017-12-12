package sculptor
package iots

import org.specs2._
import scala.xml._
import cats._
import cats.implicits._
import org.typelevel.paiges._

object integrationSpec extends mutable.Specification {
  import testing.utils._
  import iots.xsd.{Config => XsdConfig, _}
  import iots.generator.{Config => GenConfig}
  import sculptor.xsd.{ast => x}
  import ast._

  val xsdConfig = XsdConfig(
    List(
      ImportDecl(Ident("t"), "io-ts"),
      ImportDecl(Ident("T"), "core/types")
    ),
    "xsd".some,
    List(
      ExternatType(
        x.QName.fromString("xsd:date"),
        QName.of(Ident("Date")),
        QName.of(Ident("T"), Ident("date"))
      )
    )
  )

  val genConfig = GenConfig(
    Ident("t"),
    None
  )

  def transformSchema(xsd: x.Schema[Id]): ModuleDecl = {
    transform(xsd).value(xsdConfig) match {
      case Right(v) => v
      case Left(err) => sys.error(s"Transformation error: $err")
    }
  }

  def generateSources(m: ModuleDecl): Doc = {
    generator.create(genConfig).moduleDecl(m)
  }

  "iots module" should {
    "produce iots sources from fes-1.0.xsd" >> {
      val xsd = parseXsd(
        XML.load(
          getClass.getClassLoader.getResourceAsStream("xsd/fes-1.0.xsd")
        )
      )

      println(generateSources(transformSchema(xsd)).render(80).take(5000))

      true must_=== true
    }
  }

}
