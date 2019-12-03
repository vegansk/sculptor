package sculptor.scalagen
package impl
package features

import org.typelevel.paiges._

import sculptor.ast._

trait Feature {

  def handleNewtype(n: Newtype): Result[List[Doc]] = ok(List.empty)

  def handleAlias(a: Alias): Result[List[Doc]] = ok(List.empty)

  def handleRecord(r: Record): Result[List[Doc]] = ok(List.empty)

  def handleADT(a: ADT): Result[List[Doc]] = ok(List.empty)

  def handleEnum(e: Enum): Result[List[Doc]] = ok(List.empty)

  def handlePackage(p: Package): Result[List[Doc]] = ok(List.empty)

}
