import sbt._
import Keys._

object Dependencies {
  object Versions {
    val sbt = "1.2.8"
    val scala212 = "2.12.11"
    val scalaXml = "1.2.0"
    val scalaGraph = "1.13.0"
    val specs2 = "4.7.1"
    val cats = "2.0.0"
    val catsEffect = "2.0.0"
    val shapeless = "2.3.3"
    val pprint = "0.5.5"
    val kindProjector = "0.10.3"
    val paiges = "0.2.2-SNAPSHOT"
    val scalacheck = "1.14.2"
  }

  val scalaXml = "org.scala-lang.modules" %% "scala-xml" % Versions.scalaXml
  val scalaGraphCore = "org.scala-graph" %% "graph-core" % Versions.scalaGraph
  val specs2 = "org.specs2" %% "specs2-core" % Versions.specs2
  val specs2ScalaCheck = "org.specs2" %% "specs2-scalacheck" % Versions.specs2
  val cats = "org.typelevel" %% "cats-core" % Versions.cats
  val catsEffect = "org.typelevel" %% "cats-effect" % Versions.catsEffect
  val shapeless = "com.chuusai" %% "shapeless" % Versions.shapeless
  val pprint = "com.lihaoyi" %% "pprint" % Versions.pprint
  val kindProjector = "org.typelevel" %% "kind-projector" % Versions.kindProjector
  val paigesCore = "com.github.eldis" %% "paiges-core" % Versions.paiges
  val paigesCats = "com.github.eldis" %% "paiges-cats" % Versions.paiges
  val scalacheck = "org.scalacheck" %% "scalacheck" % Versions.scalacheck

  val ast = Seq(
    cats,
    paigesCore,
    scalaGraphCore,
  ) ++ Seq(
    specs2
  ).map(_ % "test")

  val xsd = Seq(
    scalaXml,
    cats,
    shapeless
  ) ++ Seq(
    specs2,
    pprint
  ).map(_ % "test")

  val common = Seq(
    cats,
    paigesCore
  )

  val tsgen = Seq(
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

  val scalagen = Seq(
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
