import sbt._
import Keys._

object Dependencies {
  object Versions {
    val scala212 = "2.12.3"
    val scalaXml = "1.0.6"
    val specs2 = "3.9.2"
    val cats = "1.0.0-MF"
    val shapeless = "2.3.2"
    val monocle = "1.5.0-cats-M1"
    val pprint = "0.5.3"
    val kindProjector = "0.9.4"
  }

  val scalaXml = "org.scala-lang.modules" %% "scala-xml" % Versions.scalaXml
  val specs2 = "org.specs2" %% "specs2-core" % Versions.specs2 % "test"
  val cats = "org.typelevel" %% "cats-core" % Versions.cats
  val shapeless = "com.chuusai" %% "shapeless" % Versions.shapeless
  val monocleCore = "com.github.julien-truffaut" %% "monocle-core" % Versions.monocle
  val monocleMacro = "com.github.julien-truffaut" %% "monocle-macro" % Versions.monocle
  val pprint = "com.lihaoyi" %% "pprint" % Versions.pprint % "test"
  val kindProjector = "org.spire-math" %% "kind-projector" % Versions.kindProjector

  val core = Seq(
    scalaXml,
    cats,
    shapeless,
    monocleCore,
    monocleMacro,

    specs2,
    pprint
  )

}
