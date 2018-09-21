package sculptor.scalagen.impl

import org.typelevel.paiges._

import sculptor.ast._

object AliasGen extends GenHelpers {

  def generate(a: Alias): Result[Doc] =
    for {
      `type` <- ok(
        createTypeExpr(a.name.name, a.parameters.map(createGenericParam))
      )
      alias <- TypeRefGen.generate(a.baseType)
    } yield Doc.text("type ") + `type` + Doc.text(" = ") + alias
}
