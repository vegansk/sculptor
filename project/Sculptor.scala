import sbt._
import sbt.ScriptedPlugin.autoImport._
import Keys._
import wartremover._

object Sculptor {

  object Config {

    type PC = Project => Project

    val commonOptions = Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8", // Specify character encoding used by source files.
    "-explaintypes", // Explain type errors in more detail.
      "-feature", // Emit warning and location for usages of features that should be imported explicitly.
      "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
      "-language:experimental.macros", // Allow macro definition (besides implementation and application)
      "-language:higherKinds", // Allow higher-kinded types
      "-language:implicitConversions", // Allow definition of implicit functions called views
      "-unchecked", // Enable additional warnings where generated code depends on assumptions.
      "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
      "-Xfatal-warnings", // Fail the compilation if there are any warnings.
      "-Xfuture", // Turn on future language features.
      "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
      "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
      "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
      "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
      "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
      "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
      "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
      "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
      "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
      "-Xlint:option-implicit", // Option.apply used implicit view.
      "-Xlint:package-object-classes", // Class or object defined in package object.
      "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
      "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
      "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
      "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
      "-Xlint:unsound-match", // Pattern match may not be typesafe.
      "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
      "-Ypartial-unification", // Enable partial unification in type constructor inference
      //TODO: Enable in production
      //"-Ywarn-dead-code", // Warn when dead code is identified.
      "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
      "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
      "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
      "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
      "-Ywarn-numeric-widen", // Warn when numerics are widened.
      "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
      "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
      "-Ywarn-unused:locals", // Warn if a local definition is unused.
      "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
      "-Ywarn-unused:privates", // Warn if a private member is unused.
      "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
    )

    val common: PC = _.settings(
      scalaVersion := Dependencies.Versions.scala212,
      organization := "com.github.vegansk",
      scalacOptions ++= commonOptions,
      resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      wartremoverErrors in (Compile, compile) ++= Warts.allBut(
        // Allow defaul arguments
        Wart.DefaultArguments,
        // See https://github.com/wartremover/wartremover/issues/263
        Wart.Any,
        Wart.Nothing,
        // Parsers are recursive
        Wart.Recursion,
        // We need implicit conversions for DSL
        Wart.ImplicitConversion
      ),
      scalacOptions in (Compile, console) ~= {
        _.filterNot(Set(
          "-Xfatal-warnings",
          "-Xlint",
          "-Ywarn-unused:imports"
        ))
      },
      addCompilerPlugin(Dependencies.kindProjector)
    )

    val publish: PC = _.settings(
      publishMavenStyle := true,
      credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
      publishTo := {
        val nexus = "http://nexus.eldissoft.lan/nexus/content/repositories/"
        if (isSnapshot.value)
          Some("snapshots" at nexus + "snapshots")
        else
          Some("releases" at nexus + "releases")
      }
    )

    val plugin: PC = _.settings(
      sbtPlugin := true,
      sbtVersion := Dependencies.Versions.sbt,
      scriptedLaunchOpts ++= Seq("-Xmx1024M", "-XX:MaxPermSize=256M", "-Dplugin.version=" + version.value),
      scriptedBufferLog := false
    )

    val ast: PC = _.configure(common, publish)
      .settings(
        name := "sculptor-ast",
        libraryDependencies ++= Dependencies.ast
      )

    val xsd: PC = _.configure(common, publish)
      .settings(
        name := "sculptor-xsd",
        libraryDependencies ++= Dependencies.xsd
      )

    val tsgen: PC = _.configure(common, publish)
      .settings(
        name := "sculptor-tsgen",
        libraryDependencies ++= Dependencies.tsgen
      )

    val sbtTsgen: PC = _.configure(common, plugin, publish)
      .settings(
        name := "sbt-sculptor-tsgen",
      )

    val scalagen: PC = _.configure(common, publish)
      .settings(
        name := "sculptor-scalagen",
        libraryDependencies ++= Dependencies.scalagen
      )

    val sbtScalagen: PC = _.configure(common, plugin, publish)
      .settings(
        name := "sbt-sculptor-scalagen",
        )

    val sbtSculptor: PC = _.configure(common, plugin, publish)
      .settings(
        name := "sbt-sculptor",
      )
  }

  lazy val ast = project
    .in(file("ast"))
    .configure(Config.ast)

  lazy val xsd = project
    .in(file("xsd"))
    .configure(Config.xsd)

  lazy val tsgen = project
    .in(file("tsgen"))
    .dependsOn(xsd)
    .configure(Config.tsgen)

  lazy val sbtTsgen = project
    .in(file("sbt-tsgen"))
    .configure(Config.sbtTsgen)
    .dependsOn(tsgen)

  lazy val scalagen = project
    .in(file("scalagen"))
    .dependsOn(xsd, ast)
    .configure(Config.scalagen)

  lazy val sbtScalagen = project
    .in(file("sbt-scalagen"))
    .configure(Config.sbtScalagen)
    .dependsOn(scalagen)

  lazy val sbtSculptor = project
    .in(file("sbt-sculptor"))
    .configure(Config.sbtSculptor)
    .dependsOn(scalagen, tsgen)

}
