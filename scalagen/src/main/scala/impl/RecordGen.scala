package sculptor.scalagen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._

object RecordGen extends GenHelpers {

  def generate(r: Record): Result[Doc] =
    for {

      indent <- getIndent

      genComments <- getGenerateComments

      typ = createTypeExpr(r.name, r.parameters)

      comment = Option(genComments)
        .filter(identity)
        .map(_ => typeComment(r, typ))

      fields = r.fields.toList.map(createField(genComments))

      body = Doc.intercalate(fieldDelim, fields)

      record = bracketBy(body)(caseClassPrefix(typ), caseClassPostfix, indent)

      features <- features.collectFeatures(_.handleRecord(r))

      result = comment.toList ++ List(record) ++ features.toNel.map { f =>
        val prefix = objectPrefix(r.name)
        bracketBy(
          Doc
            .intercalate(dblLine, f.toList)
        )(prefix, objectPostfix, indent)

      }.toList
    } yield Doc.intercalate(dblLine, result)

}
