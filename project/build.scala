import sbt._
import Keys._

object Sculptor {
  object Version {
    val scala212 = "2.12.3"
    val scalaXml = "1.0.6"
    val specs2 = "3.9.2"
    val cats = "1.0.0-MF"
    val shapeless = "2.3.2"
    val monocle = "1.5.0-cats-M1"
    val pprint = "0.5.3"
  }

  object Dependencies {
    val scalaXml = "org.scala-lang.modules" %% "scala-xml" % Version.scalaXml
    val specs2 = "org.specs2" %% "specs2-core" % Version.specs2 % "test"
    val cats = "org.typelevel" %% "cats-core" % Version.cats
    val shapeless = "com.chuusai" %% "shapeless" % Version.shapeless
    val monocle = Seq(
      "com.github.julien-truffaut" %% "monocle-core" % Version.monocle,
      "com.github.julien-truffaut" %% "monocle-macro" % Version.monocle
    )
    val pprint = "com.lihaoyi" %% "pprint" % Version.pprint % "test"
  }
}
