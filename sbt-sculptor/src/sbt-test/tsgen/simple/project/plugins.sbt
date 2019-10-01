resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
addSbtPlugin("com.eltimn" % "sbt-frontend" % "1.0.1")

sys.props.get("plugin.version") match {
  case Some(x) => addSbtPlugin("com.github.vegansk" % "sbt-sculptor" % x)
  case _ => sys.error("""|The system property 'plugin.version' is not defined.
                         |Specify this property using the scriptedLaunchOpts -D.""".stripMargin)
}

// Speed up SBT initialization
updateOptions := updateOptions.value.withLatestSnapshots(false)
