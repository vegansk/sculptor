scalaVersion in ThisBuild := "2.12.4"

val npmBuild = taskKey[Unit]("Builds project via npm")
val npmStartImpl = taskKey[Unit]("Runs project via npm (internal task)")
val npmStart = taskKey[Unit]("Runs project via npm")

val xsdTsgenOptions = XsdTsgenOptions(
  imports = List(
    XsdTsgenImport("t", "io-ts"),
    XsdTsgenImport("T", "core/types"),
    ),
  types = List(
    XsdTsgenType("xs:date", "Date", "T.date"),
    XsdTsgenType("xs:time", "Date", "T.date"),
    XsdTsgenType("xs:dateTime", "Date", "T.date")
  ),
  header = Option("/** tslint:disable: all */"),
  iotsNs = "t",
  nativeTypes = true,
  generateEnumsDocumentationGetters = true,
  generatePartialTypes = true
)

lazy val root = project.in(file("."))
  .enablePlugins(FrontendPlugin, SculptorTsgenPlugin)
  .settings(
    version := "0.0.1",

    npmBuild := {
      npm.toTask(" run build").value
    },
    npmStartImpl := {
      npm.toTask(" start").value
    },

    xsdTsgenConfigurations := Seq(
      XsdTsgenConfig(
        file("xsd") / "polaris-1.0.xsd",
        file("src_managed") / "polaris-1.0.ts",
        xsdTsgenOptions
      ),
      XsdTsgenConfig(
        file("xsd") / "fes-2.0.xsd",
        file("src_managed") / "fes-2.0.ts",
        xsdTsgenOptions
      )
    ),

    npmStart := npmStartImpl.dependsOn(xsdTsgenGenerate, npmBuild).value
  )
