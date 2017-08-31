import Sculptor._

scalaVersion := Version.scala212

libraryDependencies ++= Seq(
  Dependencies.scalaXml,
  Dependencies.specs2,
  Dependencies.cats
)

scalacOptions ++= Seq(
  "-Ypartial-unification"
)
