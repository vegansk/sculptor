package sculptor
package iots
package xsd

import ast._

final case class Config(iotsModule: ImportDecl,
                        iotsExtraModule: ImportDecl,
                        imports: List[ImportDecl],
                        xsdNs: Option[String])
