package sculptor.scalagen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._

object NewtypeGen extends GenHelpers {

  def generate(n: Newtype): Result[Doc] =
    for {
      `type` <- ok(
        createTypeExpr0(n.name.name, n.parameters.map(createGenericParam))
      )

      valueType <- TypeRefGen.generate(n.baseType)

      prefix = Doc.text("final case class ") + `type` + Doc.char('(')

      body = Doc.text("value: ") + valueType

      indent <- getIndent

      newtype = body.tightBracketBy(prefix, newtypePostfix, indent)

      features <- features.collectFeatures(_.handleNewtype(n))

      result = features.toNel.fold(newtype) { f =>
        val prefix = objectPrefix(createTypeExpr(n.name.name, Nil))
        val companion = Doc
          .intercalate(dblLine, f.toList)
          .tightBracketBy(prefix, objectPostfix, indent)

        Doc.intercalate(dblLine, List(newtype, companion))
      }

    } yield result
}
