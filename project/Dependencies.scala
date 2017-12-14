import sbt._
import Keys._

object Dependencies {
  object Versions {
    val sbt = "1.0.4"
    val scala212 = "2.12.4"
    val scalaXml = "1.0.6"
    val scalaGraph = "1.12.1"
    val specs2 = "3.9.2"
    val cats = "1.0.0-RC1"
    val catsEffect = "0.5"
    val shapeless = "2.3.2"
    val pprint = "0.5.3"
    val kindProjector = "0.9.4"
    val paiges = "0.2.0"
    val scalacheck = "1.13.5"
  }

  val scalaXml = "org.scala-lang.modules" %% "scala-xml" % Versions.scalaXml
  val scalaGraphCore = "org.scala-graph" %% "graph-core" % Versions.scalaGraph
  val scalaGraphDot = "org.scala-graph" %% "graph-dot" % Versions.scalaGraph
  val specs2 = "org.specs2" %% "specs2-core" % Versions.specs2
  val specs2ScalaCheck = "org.specs2" %% "specs2-scalacheck" % Versions.specs2
  val cats = "org.typelevel" %% "cats-core" % Versions.cats
  val catsEffect = "org.typelevel" %% "cats-effect" % Versions.catsEffect
  val shapeless = "com.chuusai" %% "shapeless" % Versions.shapeless
  val pprint = "com.lihaoyi" %% "pprint" % Versions.pprint
  val kindProjector = "org.spire-math" %% "kind-projector" % Versions.kindProjector
  val paigesCore = "org.typelevel" %% "paiges-core" % Versions.paiges
  val paigesCats = "org.typelevel" %% "paiges-cats" % Versions.paiges
  val scalacheck = "org.scalacheck" %% "scalacheck" % Versions.scalacheck

  val xsd = Seq(
    scalaXml,
    cats,
    shapeless
  ) ++ Seq(
    specs2,
    pprint
  ).map(_ % "test")

  val iots = Seq(
    scalaXml,
    scalaGraphCore,
    cats,
    catsEffect,
    shapeless,
    paigesCore,
    paigesCats
  ) ++ Seq(
    specs2,
    pprint,
    scalacheck,
    specs2ScalaCheck
  ).map(_ % "test")
}
