package sculptor.tsgen.impl

import org.typelevel.paiges._

import sculptor.ast._

object RecordGen extends GenHelpers {

  def generate(r: Record): Result[Doc] =
    for {

      indent <- getIndent

      optEnc <- getOptionalEncoding

      genComment <- getGenerateComments

      typ = createTypeExpr(r.name.name, r.parameters)

      comment = Option(genComment)
        .filter(identity)
        .map(_ => typeComment(r, typ))

      fields = r.fields.toList.map(f => createField(genComment)(f, optEnc))

      body = Doc.intercalate(line, fields)

      record = exported(
        body.tightBracketBy(interfacePrefix(typ), interfacePostfix, indent)
      )

      features <- features.collectFeatures(_.handleRecord(r))

    } yield Doc.intercalate(dblLine, comment.toList ++ List(record) ++ features)

}
