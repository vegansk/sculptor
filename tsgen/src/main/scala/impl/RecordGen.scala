package sculptor.tsgen.impl

import org.typelevel.paiges._

import sculptor.ast._

object RecordGen extends GenHelpers {

  def generate(r: Record): Result[Doc] =
    for {

      indent <- getIndent

      optEnc <- getOptionalEncoding

      genComment <- getGenerateComments

      typ = createTypeExpr0(r.name.name, r.parameters.map(createGenericParam))

      comment = Option(genComment)
        .filter(identity)
        .map(_ => typeComment(r, typ))

      fields = r.fields.toList.map(f => createField(genComment)(f, optEnc))

      body = Doc.intercalate(line, fields)

      record = exported(
        bracketBy(body)(interfacePrefix(typ), interfacePostfix, indent)
      )

      features <- features.collectTypeFeatures(r)(_.handleRecord(r))

    } yield
      Doc.stack(
        comment.toList ++ (Doc.intercalate(dblLine, record :: features) :: Nil)
      )

}
