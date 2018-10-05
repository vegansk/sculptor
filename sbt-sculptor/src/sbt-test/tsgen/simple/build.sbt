scalaVersion in ThisBuild := "2.12.4"

lazy val generateSources = taskKey[Unit]("generateSources")

val npmBuild = taskKey[Unit]("Builds project via npm")
val npmStartImpl = taskKey[Unit]("Runs project via npm (internal task)")
val npmStart = taskKey[Unit]("Runs project via npm")

lazy val root = project.in(file("."))
  .enablePlugins(FrontendPlugin, SculptorPlugin)
  .settings(
    version := "0.0.1",

    npmBuild := {
      npm.toTask(" run build").value
    },
    npmStartImpl := {
      npm.toTask(" start").value
    },

    tsgenConfigurations ++= Seq(
      TsgenConfig(
        Simple.packageAst,
        TypeScript.Config(
          features = List(
            TypeScript.Feature.Constructors,
            TypeScript.Feature.IoTsTypes(iotsNs = "t")
          ),
          prefixCode = """|import { Option } from "fp-ts/lib/Option"
                          |import * as t from "io-ts"""".stripMargin,
          optionalEncoding = TypeScript.OptionalEncoding("Option")
        ),
        file("src_managed") / "types.ts"
      )
    ),

    generateSources := {
      tsgenGenerate.value
    },

    npmStart := npmStartImpl.dependsOn(generateSources, npmBuild).value

  )
