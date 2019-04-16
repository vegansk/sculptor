package sculptor.scalagen.deprecated

import org.specs2._
import scala.xml._
import cats._
import cats.implicits._
import org.typelevel.paiges._

object integrationSpec extends mutable.Specification {
  import sculptor.scalagen.testing.utils._
  import sculptor.scalagen.deprecated.xsd.{Config => XsdConfig, _}
  import generator.{
    Config => GenConfig,
    Parameters => GenParameters,
    ExternalType => GenType,
    _
  }
  import sculptor.xsd.{ast => x}
  import ast._

  def xsdConfig(ns: String) = XsdConfig(
    List(ImportDecl("java.time.LocalDate"), ImportDecl("scala.xml._")),
    ns.some,
    List(
      ExternalType(
        x.QName.fromString(s"$ns:date"),
        QName.of(Ident("LocalDate"))
      ),
      ExternalType(x.QName.fromString(s"$ns:anyType"), QName.of(Ident("Node"))),
      ExternalType(
        x.QName.fromString(s"$ns:dateTime"),
        QName.of(Ident("LocalDateTime"))
      ),
      ExternalType(
        x.QName.fromString(s"$ns:time"),
        QName.of(Ident("LocalTime"))
      ),
      ExternalType(x.QName("official_info_t", None), QName.of(Ident("fake")))
    )
  )

  val genConfig = GenConfig(
    Some("com.github.vegansk"),
    None,
    List(GenType("LocalDate")),
    GenParameters(
      generateComments = true,
      generateXmlSerializers = false,
      generateKantanXPathDecoders = true,
      generateCirceCodecs = false,
      generateOptionalTypes = OptionalTypes.Generate(
        "strong".some,
        Map("Document" -> List("messageHeader", "services"))
      )
    )
  )

  def transformSchema(xsd: x.Schema[Id], ns: String = "xs"): ModuleDecl = {
    transform(xsd).value(xsdConfig(ns)) match {
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
        XML.load(getClass.getClassLoader.getResourceAsStream("xsd/fes-2.0.xsd"))
      )

      // println(generateSources(transformSchema(xsd)).render(80).take(600000))
      val _ = generateSources(transformSchema(xsd)).render(0)

      true must_=== true
    }

    "produce scala sources from ca_iso20022_v2.xsd" >> {
      val xsd = parseXsd(
        XML.load(
          getClass.getClassLoader.getResourceAsStream("xsd/ca_iso20022_v2.xsd")
        )
      )

      // println(generateSources(transformSchema(xsd, "xsd")).render(80).take(600000))
      val _ = generateSources(transformSchema(xsd, "xsd")).render(0)

      true must_=== true
    }
  }
}
