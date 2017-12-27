scalaVersion in ThisBuild := "2.12.4"

lazy val generateSources = taskKey[Unit]("generateSources")

val cats = "org.typelevel" %% "cats-core" % "1.0.0-RC1"
val circe = "io.circe" %% "circe-core" % "0.9.0-M2"
val circeJava8 = "io.circe" %% "circe-java8" % "0.9.0-M2"
val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.0.6"

lazy val root = project.in(file("."))
  .enablePlugins(SculptorScalagenPlugin)
  .settings(
    version := "0.0.1",

    libraryDependencies ++= Seq(
      cats, circe, circeJava8, scalaXml
    ),

    scalagenConfig := ScalagenConfig(
      packageName = Option("polaris.schema.fes"),
      imports = List(
        ScalagenImport("java.time.LocalDate"),
        ScalagenImport("java.util.UUID"),
        ScalagenImport("cats._"),
        ScalagenImport("cats.implicits._"),
        ScalagenImport("io.circe._"),
        ScalagenImport("io.circe.syntax._"),
        ScalagenImport("scala.xml._"),
        ScalagenImport("polaris.json.instances._")
      ),
      types = List(
        ScalagenType("xs:date", "LocalDate"),
        ScalagenType("uuid_t", "UUID")
      ),
      header = Option("/* This file is autogenerated. DO NOT EDIT! */"),
      parameters = ScalagenParameters(
        generateComments = true,
        generateCatsEq = true,
        generateCirceCodecs = true,
        generateXmlSerializers = true,
        generateOptionalTypes = false
      )
    ),

    scalagenTargetDirectory := file("src/main/scala"),

    scalagenXsdFiles := Seq(
      file("xsd") / "fes-2.0.xsd"
    ),

    generateSources := {
      scalagenGenerate.value
    },

    compile.in(Compile) := compile.in(Compile).dependsOn(generateSources).value

  )
