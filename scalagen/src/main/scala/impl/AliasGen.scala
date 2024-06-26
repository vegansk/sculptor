package sculptor.scalagen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._

object AliasGen extends GenHelpers {

  import ScalaIdent._

  def generate(a: Alias): Result[Doc] =
    for {
      indent <- getIndent

      genComments <- getGenerateComments

      typ = createTypeExpr00(
        a.name.asScalaId,
        a.parameters.map(createGenericParam)
      )

      comment = doc(genComments)(typeComment(a, typ))

      alias = Doc.text("type ") + typ + Doc.text(" = ") + createTypeRef(
        a.baseType
      )

      features <- features.collectTypeFeatures(a)(_.handleAlias(a))

    } yield
      Doc.stack(
        comment.toList ++ (
          Doc.intercalate(
            dblLine,
            alias :: features.toNel.map { f =>
              val prefix = objectPrefix(a.name)
              Doc
                .intercalate(dblLine, f.toList)
                .tightBracketBy(prefix, objectPostfix, indent)
            }.toList
          ) :: Nil
        )
      )
}
