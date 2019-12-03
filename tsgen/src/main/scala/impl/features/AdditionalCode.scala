package sculptor.tsgen
package impl
package features

import cats.data.NonEmptyList
import cats.implicits._
import org.typelevel.paiges.Doc
import sculptor.ast._

object AdditionalCode extends Feature {

  private def genAdditionalCode(
    code: Option[NonEmptyList[Doc]]
  ): Result[List[Doc]] =
    ok(code.toList.traverse(_.toList).flatten)

  override def handleNewtype(n: Newtype): Result[List[Doc]] =
    genAdditionalCode(n.additionalCode)

  override def handleAlias(a: Alias): Result[List[Doc]] =
    genAdditionalCode(a.additionalCode)

  override def handleRecord(r: Record): Result[List[Doc]] =
    genAdditionalCode(r.additionalCode)

  override def handleADT(a: ADT): Result[List[Doc]] =
    genAdditionalCode(a.additionalCode)

  override def handleEnum(e: Enum): Result[List[Doc]] =
    genAdditionalCode(e.additionalCode)

  override def handlePackage(p: Package): Result[List[Doc]] =
    genAdditionalCode(p.additionalCode)
}
