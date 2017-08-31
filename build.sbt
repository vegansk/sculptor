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

wartremoverErrors in (Compile, compile) ++= Warts.allBut(
  // Allow defaul arguments
  Wart.DefaultArguments,
  // See https://github.com/wartremover/wartremover/issues/263
  Wart.Any,
  Wart.Nothing,
  // Parsers are recursive
  Wart.Recursion
)
