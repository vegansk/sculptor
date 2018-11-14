package sculptor.scalagen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._

object AliasGen extends GenHelpers {

  def generate(a: Alias): Result[Doc] =
    for {
      indent <- getIndent

      genComments <- getGenerateComments

      typ = createTypeExpr0(a.name.name, a.parameters.map(createGenericParam))

      comment = doc(genComments)(typeComment(a, typ))

      alias = Doc.text("type ") + typ + Doc.text(" = ") + createTypeRef(
        a.baseType
      )

      features <- features.collectFeatures(_.handleAlias(a))

      result = comment.toList ++ List(alias) ++ features.toNel.map { f =>
        val prefix = objectPrefix(createTypeExpr(a.name.name, Nil))
        Doc
          .intercalate(dblLine, f.toList)
          .tightBracketBy(prefix, objectPostfix, indent)
      }.toList
    } yield Doc.intercalate(dblLine, result)
}
