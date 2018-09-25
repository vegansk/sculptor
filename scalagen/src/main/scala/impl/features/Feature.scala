package sculptor.scalagen
package impl
package features

import org.typelevel.paiges._

import sculptor.ast._

trait Feature {

  def handleNewtype(n: Newtype): Result[Option[Doc]] = ok(None)

  def handleAlias(a: Alias): Result[Option[Doc]] = ok(None)

  def handleRecord(r: Record): Result[Option[Doc]] = ok(None)

  def handleADT(a: ADT): Result[Option[Doc]] = ok(None)

  def handleEnum(e: Enum): Result[Option[Doc]] = ok(None)

}
