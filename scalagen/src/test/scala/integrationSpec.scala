package sculptor
package scalagen

import org.specs2._
import scala.xml._
import cats._
import cats.implicits._
import org.typelevel.paiges._

object integrationSpec extends mutable.Specification {
  import testing.utils._
  import scalagen.xsd.{Config => XsdConfig, _}
  import scalagen.generator.{
    Config => GenConfig, Parameters => GenParameters, ExternalType => GenType, _
  }
  import sculptor.xsd.{ast => x}
  import ast._

  val xsdConfig = XsdConfig(
    List(
      ImportDecl("java.time.LocalDate")
    ),
    "xs".some,
    List(
      ExternalType(
        x.QName.fromString("xs:date"),
        QName.of(Ident("LocalDate"))
      ),
      ExternalType(
        x.QName("official_info_t", None),
        QName.of(Ident("fake"))
      )
    )
  )

  val genConfig = GenConfig(
    Some("com.github.vegansk"),
    None,
    List(GenType("LocalDate")),
    GenParameters(
      generateXmlSerializers = true,
      generateCirceCodecs = true,
      generateOptionalTypes = OptionalTypes.Generate(
        "strong".some,
        Map("Document" -> List("messageHeader", "services"))
      )
    )
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

  "scala module" should {
    "produce scala sources from fes-2.0.xsd" >> {
      val xsd = parseXsd(
        XML.load(
          getClass.getClassLoader.getResourceAsStream("xsd/fes-2.0.xsd")
        )
      )

      // println(generateSources(transformSchema(xsd)).render(80).take(6000000))
      val _ = generateSources(transformSchema(xsd)).render(0)

      true must_=== true
    }
  }
}
