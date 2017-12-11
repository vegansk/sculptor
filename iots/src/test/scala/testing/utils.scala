package sculptor
package iots
package testing

import scala.xml._
import cats._

object utils {

  import sculptor.xsd.{ast => x, schemaAst}

  def parseXsd(xsd: Node): x.Schema[Id] =
    schemaAst(xsd).getOrElse(sys.error("Can't parse schema"))

  def xsd(child: NodeSeq): Node =
    <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" elementFormDefault="qualified">
      {child}
    </xs:schema>

  def parseXsdTypes(child: NodeSeq): List[x.Type[Id]] =
    parseXsd(xsd(child)).types

}
