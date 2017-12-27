package sculptor
package scalagen
package generator

final case class Parameters(generateComments: Boolean = true,
                            generateCatsEq: Boolean = false,
                            generateCirceCodecs: Boolean = false,
                            generateXmlSerializers: Boolean = false,
                            generateOptionalTypes: Boolean = false)

final case class ExternalType(name: String) extends AnyVal

final case class Config(packageName: Option[String],
                        header: Option[String],
                        externalTypes: List[ExternalType],
                        parameters: Parameters)
