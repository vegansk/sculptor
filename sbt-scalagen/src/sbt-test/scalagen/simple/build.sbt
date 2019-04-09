scalaVersion in ThisBuild := "2.12.4"

lazy val generateSources = taskKey[Unit]("generateSources")

val cats = "org.typelevel" %% "cats-core" % "1.4.0"
val circe = "io.circe" %% "circe-core" % "0.11.1"
val circeJava8 = "io.circe" %% "circe-java8" % "0.11.1"
val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
val kantanXPath = "com.nrinaudo" %% "kantan.xpath" % "0.5.0"
val kantanXPathJava8 = "com.nrinaudo" %% "kantan.xpath-java8" % "0.5.0"

val caOptions = XsdScalagenOptions(
  packageName = Option("corpact.schema"),
  imports = List(
    XsdScalagenImport("java.time._"),
    XsdScalagenImport("cats.implicits._"),
    XsdScalagenImport("scala.xml._"),
    XsdScalagenImport("kantan.xpath.{NodeDecoder, codecs, Query}"),
    XsdScalagenImport("kantan.xpath.implicits._"),
    XsdScalagenImport("kantan.xpath.java8._"),
    XsdScalagenImport("kantan.codecs.strings.StringDecoder"),
    XsdScalagenImport("corpact.schema.cacodecs._")
  ),
  types = List(
    XsdScalagenType("xsd:date", "LocalDate"),
    XsdScalagenType("xsd:time", "LocalTime"),
    XsdScalagenType("xsd:dateTime", "ZonedDateTime"),
    XsdScalagenType("xsd:anyType", "NodeSeq"),
  ),
  header = Option("/* This file is autogenerated. DO NOT EDIT! */"),
  parameters = XsdScalagenParameters(
    generateComments = true,
    generateCatsEq = false,
    generateCirceCodecs = false,
    generateXmlSerializers = true,
    generateKantanXPathDecoders = true,
    generateOptionalTypes = XsdScalagenOptionalTypes.No
  )
)

val xsdScalagenOptions = XsdScalagenOptions(
  packageName = Option("polaris.schema.fes"),
  imports = List(
    XsdScalagenImport("scala.xml._"),
    XsdScalagenImport("cats._"),
    XsdScalagenImport("cats.implicits._"),
    XsdScalagenImport("java.time.LocalDate"),
    XsdScalagenImport("java.util.UUID"),
    XsdScalagenImport("io.circe._"),
    XsdScalagenImport("io.circe.syntax._"),
    XsdScalagenImport("kantan.xpath.{NodeDecoder, codecs, Query}"),
    XsdScalagenImport("kantan.xpath.implicits._"),
    XsdScalagenImport("kantan.codecs.strings.StringDecoder"),
    XsdScalagenImport("kantan.xpath.java8._")
  ),
  types = List(
    XsdScalagenType("xs:date", "LocalDate"),
    XsdScalagenType("uuid_t", "UUID")
  ),
  header = Option("/* This file is autogenerated. DO NOT EDIT! */"),
  parameters = XsdScalagenParameters(
    generateComments = true,
    generateCatsEq = true,
    generateCirceCodecs = true,
    generateXmlSerializers = true,
    generateKantanXPathDecoders = true,
    generateOptionalTypes = XsdScalagenOptionalTypes.No
  )
)

val xsdScalagenOptionalOptions = xsdScalagenOptions.copy(
  imports = List(
    XsdScalagenImport("java.time.LocalDate"),
    XsdScalagenImport("java.util.UUID"),
    XsdScalagenImport("cats._"),
    XsdScalagenImport("cats.data._"),
    XsdScalagenImport("cats.implicits._"),
    XsdScalagenImport("io.circe._"),
    XsdScalagenImport("io.circe.syntax._")
  ),
  packageName = Option("polaris.schema.fes.optional"),
  parameters = xsdScalagenOptions.parameters.copy(
    generateXmlSerializers = false,
    generateKantanXPathDecoders = false,
    generateOptionalTypes = XsdScalagenOptionalTypes.Generate(
      Some("polaris.schema.fes"),
      Map(
        "Document" -> List(
          "messageHeader", "services"
        )
      )
    )
  )
)

lazy val root = project.in(file("."))
  .enablePlugins(SculptorScalagenPlugin)
  .settings(
    version := "0.0.1",

    libraryDependencies ++= Seq(
      cats, circe, circeJava8, scalaXml, kantanXPath, kantanXPathJava8
    ),

    scalacOptions ++= Seq(
      "-Ypartial-unification"
    ),

    xsdScalagenConfigurations := Seq(
      XsdScalagenConfig(
        file("xsd") / "fes-2.0.xsd",
        file("src") / "main/scala/fes-2.0.scala",
        xsdScalagenOptions
      ),
      XsdScalagenConfig(
        file("xsd") / "fes-2.0.xsd",
        file("src") / "main/scala/fes-2.0-optional.scala",
        xsdScalagenOptionalOptions
      ),
      XsdScalagenConfig(
        file("xsd") / "ca_iso20022_v2.xsd",
        file("src") / "main/scala/ca.scala",
        caOptions
      )
    ),

    generateSources := {
      xsdScalagenGenerate.value
    },

    compile.in(Compile) := compile.in(Compile).dependsOn(generateSources).value

  )
