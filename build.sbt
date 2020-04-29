import Sculptor._

scalaVersion.in(ThisBuild) := Dependencies.Versions.scala212

lazy val ast = Sculptor.ast
lazy val xsd = Sculptor.xsd
lazy val common = Sculptor.common
lazy val tsgen = Sculptor.tsgen
lazy val sbtTsgen = Sculptor.sbtTsgen
lazy val scalagen = Sculptor.scalagen
lazy val sbtScalagen = Sculptor.sbtScalagen
lazy val sbtsculptor = Sculptor.sbtSculptor

skip in publish := true
