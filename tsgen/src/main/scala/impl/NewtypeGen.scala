package sculptor.tsgen.impl

import org.typelevel.paiges._

import sculptor.ast._

object NewtypeGen extends GenHelpers {

  def generate(n: Newtype): Result[Doc] =
    for {
      indent <- getIndent

      typ = createTypeExpr0(n.name.name, n.parameters.map(createGenericParam))

      valueType = createTypeRef(n.baseType)

      prefix = Doc.text("interface ") + typ + Doc.text(" extends ") + valueType + Doc
        .text(" {")

      body = createBrandField(typ)

      newtype = body.tightBracketBy(prefix, newtypePostfix, indent)

      features <- features.collectFeatures(_.handleNewtype(n))

    } yield Doc.intercalate(dblLine, newtype :: features)
}
