package sculptor
package scala
package generator

final case class Config(packageName: Option[String],
                        header: Option[String],
                        generateComments: Boolean)
