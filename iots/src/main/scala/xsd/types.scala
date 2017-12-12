package sculptor
package iots
package xsd

import sculptor.xsd.{ast => x}
import ast._

final case class ExternatType(xsdName: x.QName, name: QName, constName: QName)

final case class Config(imports: List[ImportDecl],
                        xsdNs: Option[String],
                        externalTypes: List[ExternatType])
