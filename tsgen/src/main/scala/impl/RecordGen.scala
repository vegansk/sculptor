package sculptor.tsgen.impl

import org.typelevel.paiges._

import sculptor.ast._

object RecordGen extends GenHelpers {

  def generate(r: Record): Result[Doc] =
    for {

      indent <- getIndent

      typ = createTypeExpr(r.name.name, r.parameters)

      fields = r.fields.toList.map(f => createField(f))

      body = Doc.intercalate(line, fields)

      record = exported(
        body.tightBracketBy(interfacePrefix(typ), interfacePostfix, indent)
      )

      features <- features.collectFeatures(_.handleRecord(r))

    } yield Doc.intercalate(dblLine, record :: features)

}
