scalaVersion in ThisBuild := "2.12.4"

val npmBuild = taskKey[Unit]("Builds project via npm")
val npmStartImpl = taskKey[Unit]("Runs project via npm (internal task)")
val npmStart = taskKey[Unit]("Runs project via npm")

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

    tsgenConfig := TsgenConfig(
      imports = List(
        TsgenImport("tsgen", "io-ts"),
        TsgenImport("T", "core/types"),
      ),
      types = List(
        TsgenType("xs:date", "Date", "T.date"),
        TsgenType("xs:dateTime", "Date", "T.date")
      ),
      header = Option("/** tslint:disable: all */"),
      iotsNs = "iots",
      nativeTypes = true,
      generateEnumsDocumentationGetters = true,
      generatePartialTypes = true
    ),

    tsgenTargetDirectory := file("src_generated"),

    tsgenXsdFiles := Seq(
      file("xsd") / "polaris-1.0.xsd",
      file("xsd") / "fes-2.0.xsd"
    ),

    npmStart := npmStartImpl.dependsOn(tsgenGenerate, npmBuild).value
  )
