package sculptor.scalagen.impl

import org.typelevel.paiges._

import sculptor.ast._

object RecordGen extends GenHelpers {

  def generate(r: Record): Result[Doc] =
    for {

      indent <- getIndent

      typ = createTypeExpr(r.name.name, r.parameters.map(createGenericParam))

      fields = r.fields.toList.map(f => createField(f))

      body = Doc.intercalate(fieldDelim, fields)

    } yield body.tightBracketBy(caseClassPrefix(typ), caseClassPostfix, indent)

}
