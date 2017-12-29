package sculptor
package scalagen
package generator

sealed trait OptionalTypes {
  val toBool: Boolean
  val strongTypesPrefix: Option[String]
}
object OptionalTypes {
  object No extends OptionalTypes {
    val toBool = false
    val strongTypesPrefix = None
  }
  final case class Generate(val strongTypesPrefix: Option[String] = None)
      extends OptionalTypes {
    val toBool = true
  }
}

final case class Parameters(generateComments: Boolean = true,
                            generateCatsEq: Boolean = false,
                            generateCirceCodecs: Boolean = false,
                            generateXmlSerializers: Boolean = false,
                            generateOptionalTypes: OptionalTypes =
                              OptionalTypes.No)

final case class ExternalType(name: String) extends AnyVal

final case class Config(packageName: Option[String],
                        header: Option[String],
                        externalTypes: List[ExternalType],
                        parameters: Parameters)
