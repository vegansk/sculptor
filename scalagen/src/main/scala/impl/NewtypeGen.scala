package sculptor.scalagen.impl

import org.typelevel.paiges._

import sculptor.ast._

object NewtypeGen extends GenHelpers {

  def generate(n: Newtype): Result[Doc] =
    for {
      `type` <- ok(
        createTypeExpr(n.name.name, n.parameters.map(createGenericParam))
      )

      valueType <- TypeRefGen.generate(n.baseType)

      prefix = Doc.text("final case class ") + `type` + Doc.char('(')

      body = Doc.text("value: ") + valueType

      indent <- getIndent

    } yield body.tightBracketBy(prefix, Doc.char(')'), indent)
}
