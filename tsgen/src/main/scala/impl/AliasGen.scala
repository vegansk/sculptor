package sculptor.tsgen.impl

import org.typelevel.paiges._

import sculptor.ast._

object AliasGen extends GenHelpers {

  def generate(a: Alias): Result[Doc] =
    for {
      features <- features.collectFeatures(_.handleAlias(a))

      typ = createTypeExpr0(a.name.name, a.parameters.map(createGenericParam))
      baseType = createTypeRef(a.baseType)
      alias = Doc.text("type ") + typ + Doc.text(" = ") + baseType

    } yield Doc.intercalate(dblLine, alias :: features)
}
