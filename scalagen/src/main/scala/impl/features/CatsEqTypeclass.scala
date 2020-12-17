package sculptor.scalagen.impl
package features

import org.typelevel.paiges._

import sculptor.ast._
import sculptor.scalagen.Feature

object CatsEqTypeclass extends Feature with GenHelpers {

  private def fromUniversalEquals(r: TypeRef): Result[Doc] =
    ok(createTypeclassDef(r, "Eq", false).space("Eq.fromUniversalEquals"))

  override def handleNewtype(n: Newtype) =
    fromUniversalEquals(n.ref).map(List(_))

  override def handleRecord(r: Record) =
    fromUniversalEquals(r.ref).map(List(_))

  override def handleADT(a: ADT) =
    fromUniversalEquals(a.ref).map(List(_))

  override def handleEnum(e: Enum) =
    fromUniversalEquals(e.ref).map(List(_))

}
