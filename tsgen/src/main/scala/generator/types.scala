package sculptor
package tsgen
package generator

import ast._

final case class Config(iotsNs: Ident,
                        header: Option[String],
                        nativeTypes: Boolean,
                        generateComments: Boolean,
                        generateEnumsDocumentationGetters: Boolean,
                        generatePartialTypes: Boolean,
                        generatePartialConstants: Boolean)
