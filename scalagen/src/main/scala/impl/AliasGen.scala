package sculptor.scalagen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._

object AliasGen extends GenHelpers {

  def generate(a: Alias): Result[Doc] =
    for {
      indent <- getIndent

      typ = createTypeExpr0(a.name.name, a.parameters.map(createGenericParam))
      alias = Doc.text("type ") + typ + Doc.text(" = ") + createTypeRef(
        a.baseType
      )

      features <- features.collectFeatures(_.handleAlias(a))

      result = features.toNel.fold(alias) { f =>
        val prefix = objectPrefix(createTypeExpr(a.name.name, Nil))
        val companion = Doc
          .intercalate(dblLine, f.toList)
          .tightBracketBy(prefix, objectPostfix, indent)

        Doc.intercalate(dblLine, List(alias, companion))
      }
    } yield result
}
