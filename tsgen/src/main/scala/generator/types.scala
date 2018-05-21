package sculptor
package tsgen
package generator

final case class Config(iotsNs: String,
                        header: Option[String] = None,
                        customIotsType: Option[String] = None,
                        nativeTypes: Boolean = true,
                        generateComments: Boolean = true,
                        generateEnumsDocumentationGetters: Boolean = false,
                        generatePartialTypes: Boolean = false,
                        generatePartialConstants: Boolean = false)
