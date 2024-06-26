package sculptor.tsgen.impl

import org.typelevel.paiges._

import sculptor.ast._

object NewtypeGen extends GenHelpers {

  def generate(n: Newtype): Result[Doc] =
    for {
      indent <- getIndent

      typ = createTypeExpr0(n.name.name, n.parameters.map(createGenericParam))

      comment <- getGenerateComments.map(doc(_)(typeComment(n, typ)))

      valueType = createTypeRef(n.baseType)

      prefix = Doc.text("type ") + typ + Doc.text(" = ") + valueType + Doc
        .text(" & {")

      body = createBrandField(typ)

      newtype = exported(body.tightBracketBy(prefix, newtypePostfix, indent))

      features <- features.collectTypeFeatures(n)(_.handleNewtype(n))

    } yield
      Doc.stack(
        comment.toList ++ (
          Doc.intercalate(dblLine, newtype :: features) :: Nil
        )
      )
}
