package sculptor.scalagen.impl
package features

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._

object CatsEqTypeclass extends Feature with GenHelpers {

  private def fromUniversalEquals(r: TypeRef): Result[Doc] = {
    val typ = createTypeRef(r)
    val (definition, name, params) = TypeRef.cata(
      s =>
        (
          if (s.parameters.isEmpty) "val" else "def",
          s.name.name.name,
          createParameters(s.parameters.map(createTypeRef))
      ),
      g => ("val", g.name.name, Doc.empty)
    )(r)
    ok(
      Doc.text(s"implicit ${definition} ${name}Eq") + params + Doc
        .text(": Eq[") + typ + Doc.text("] = Eq.fromUniversalEquals")
    )
  }

  override def handleNewtype(n: Newtype) =
    fromUniversalEquals(n.ref).map(_.some)

  override def handleRecord(r: Record) =
    fromUniversalEquals(r.ref).map(_.some)

  override def handleADT(a: ADT) =
    fromUniversalEquals(a.ref).map(_.some)

  override def handleEnum(e: Enum) =
    fromUniversalEquals(e.ref).map(_.some)

}
