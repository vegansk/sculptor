package sculptor
package iots
package generator

import ast._

final case class Config(iotsNs: Ident,
                        header: Option[String],
                        nativeTypes: Boolean,
                        generateComments: Boolean)
