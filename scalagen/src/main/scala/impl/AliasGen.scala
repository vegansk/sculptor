package sculptor.scalagen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._

object AliasGen extends GenHelpers {

  def generate(a: Alias): Result[Doc] =
    for {
      `type` <- ok(
        createTypeExpr0(a.name.name, a.parameters.map(createGenericParam))
      )
      alias <- TypeRefGen
        .generate(a.baseType)
        .map(a => Doc.text("type ") + `type` + Doc.text(" = ") + a)

      indent <- getIndent

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
