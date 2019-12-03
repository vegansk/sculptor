scalaVersion in ThisBuild := "2.12.10"

lazy val generateSources = taskKey[Unit]("generateSources")

val cats = "org.typelevel" %% "cats-core" % "2.0.0"
val circe = "io.circe" %% "circe-core" % "0.12.2"
val circeParser = "io.circe" %% "circe-parser" % "0.12.2"
val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.2.0"
val kantanXPath = "com.nrinaudo" %% "kantan.xpath" % "0.5.1"
val kantanXPathJava8 = "com.nrinaudo" %% "kantan.xpath-java8" % "0.5.1"

lazy val root = project.in(file("."))
  .enablePlugins(SculptorPlugin)
  .settings(
    version := "0.0.1",

    libraryDependencies ++= Seq(
      cats, circe, circeParser, scalaXml, kantanXPath, kantanXPathJava8
    ),

    scalacOptions ++= Seq(
      "-Ypartial-unification"
    ),

    scalagenConfigurations ++= Seq(
      ScalagenConfig(
        Simple.packageAst,
        Scala.Config(
          features = List(
            Scala.Feature.CatsEqTypeclass,
            Scala.Feature.CirceCodecs(),
            Scala.Feature.AdditionalCode
          ),
          prefixCode = """|import cats._
                          |import io.circe._
                          |import io.circe.syntax._
                          |""".stripMargin
        ),
        file("src") / "main/scala/types.scala"
      )
    ),

    generateSources := {
      scalagenGenerate.value
    },

    compile.in(Compile) := compile.in(Compile).dependsOn(generateSources).value

  )
