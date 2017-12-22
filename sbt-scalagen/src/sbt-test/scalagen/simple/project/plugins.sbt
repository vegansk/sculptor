sys.props.get("plugin.version") match {
  case Some(x) => addSbtPlugin("com.github.vegansk" % "sbt-sculptor-scalagen" % x)
  // TODO: See https://gitlab.com/vegansk/sculptor/issues/38
  case _ => addSbtPlugin("com.github.vegansk" % "sbt-sculptor-scalagen" % "0.0.1-SNAPSHOT")
  // case _ => sys.error("""|The system property 'plugin.version' is not defined.
  //                        |Specify this property using the scriptedLaunchOpts -D.""".stripMargin)
}
