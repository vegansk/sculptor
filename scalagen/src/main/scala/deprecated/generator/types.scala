package sculptor.scalagen.deprecated
package generator

sealed trait OptionalTypes {
  val toBool: Boolean
  val strongTypesPrefix: Option[String]
  val requiredFields: Map[String, List[String]]
}
object OptionalTypes {
  object No extends OptionalTypes {
    val toBool = false
    val strongTypesPrefix = None
    val requiredFields = Map()
  }
  final case class Generate(val strongTypesPrefix: Option[String] = None,
                            val requiredFields: Map[String, List[String]] =
                              Map())
      extends OptionalTypes {
    val toBool = true
  }
}

final case class Parameters(generateComments: Boolean = true,
                            generateCatsEq: Boolean = false,
                            generateCirceCodecs: Boolean = false,
                            generateXmlSerializers: Boolean = false,
                            generateKantanXPathDecoders: Boolean = false,
                            generateOptionalTypes: OptionalTypes =
                              OptionalTypes.No)

final case class ExternalType(name: String) extends AnyVal

final case class Config(packageName: Option[String],
                        header: Option[String],
                        externalTypes: List[ExternalType],
                        parameters: Parameters)
