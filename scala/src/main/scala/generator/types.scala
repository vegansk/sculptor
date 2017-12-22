package sculptor
package scala
package generator

final case class Parameters(generateComments: Boolean = true,
                            generateCatsEq: Boolean = false)

final case class Config(packageName: Option[String],
                        header: Option[String],
                        parameters: Parameters)
