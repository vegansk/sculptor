scalaVersion in ThisBuild := "2.12.4"

lazy val generateSources = taskKey[Unit]("generateSources")

val cats = "org.typelevel" %% "cats-core" % "1.4.0"
val circe = "io.circe" %% "circe-core" % "0.9.2"
val circeParser = "io.circe" %% "circe-parser" % "0.9.2"
val circeJava8 = "io.circe" %% "circe-java8" % "0.9.2"
val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
val kantanXPath = "com.nrinaudo" %% "kantan.xpath" % "0.4.0"
val kantanXPathJava8 = "com.nrinaudo" %% "kantan.xpath-java8" % "0.4.0"

lazy val root = project.in(file("."))
  .enablePlugins(SculptorPlugin)
  .settings(
    version := "0.0.1",

    libraryDependencies ++= Seq(
      cats, circe, circeParser, circeJava8, scalaXml, kantanXPath, kantanXPathJava8
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
            Scala.Feature.CirceCodecs()
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
