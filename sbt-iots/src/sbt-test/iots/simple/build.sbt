scalaVersion in ThisBuild := "2.12.4"

val npmBuild = taskKey[Unit]("Builds project via npm")
val npmStartImpl = taskKey[Unit]("Runs project via npm (internal task)")
val npmStart = taskKey[Unit]("Runs project via npm")

lazy val root = project.in(file("."))
  .enablePlugins(FrontendPlugin, SculptorIotsPlugin)
  .settings(
    version := "0.0.1",

    npmBuild := {
      npm.toTask(" run build").value
    },
    npmStartImpl := {
      npm.toTask(" start").value
    },

    iotsConfig := IotsConfig(
      imports = List(
        IotsImport("iots", "io-ts"),
        IotsImport("T", "core/types"),
      ),
      types = List(
        IotsType("xs:date", "Date", "T.date"),
        IotsType("xs:dateTime", "Date", "T.date")
      ),
      header = Option("/** tslint:disable: all */"),
      iotsNs = "iots",
      nativeTypes = true,
      generateEnumsDocumentationGetters = true,
      generatePartialTypes = true
    ),

    iotsTargetDirectory := file("src_generated"),

    iotsXsdFiles := Seq(
      file("xsd") / "polaris-1.0.xsd",
      file("xsd") / "fes-2.0.xsd"
    ),

    npmStart := npmStartImpl.dependsOn(iotsGenerate, npmBuild).value
  )
