resolvers += Resolver.jcenterRepo

addSbtPlugin("com.eltimn" % "sbt-frontend" % "1.0.1")

sys.props.get("plugin.version") match {
  case Some(x) => addSbtPlugin("com.github.vegansk" % "sbt-sculptor-tsgen" % x)
  // TODO: See https://gitlab.com/vegansk/sculptor/issues/38
  case _ => addSbtPlugin("com.github.vegansk" % "sbt-sculptor-tsgen" % "0.0.3-SNAPSHOT")
  // case _ => sys.error("""|The system property 'plugin.version' is not defined.
  //                        |Specify this property using the scriptedLaunchOpts -D.""".stripMargin)
}
