scalaVersion in ThisBuild := "2.12.10"

val npmInstall = taskKey[Unit]("Install npm dependencies")
val npmBuild = taskKey[Unit]("Builds project via npm")
val npmStart = taskKey[Unit]("Runs project via npm")

lazy val root = project.in(file("."))
  .enablePlugins(FrontendPlugin, SculptorPlugin)
  .settings(
    version := "0.0.1",

    npmInstall := npm.toTask(" install -d").value,

    npmBuild := npm.toTask(" run build")
      .dependsOn(tsgenGenerate, npmInstall)
      .value,

    npmStart := npm.toTask(" start")
      .dependsOn(npmBuild)
      .value,

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
    )
  )
