package sculptor.scalagen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._

object RecordGen extends GenHelpers {

  def generate(r: Record): Result[Doc] =
    for {

      indent <- getIndent

      typ = createTypeExpr(r.name.name, r.parameters)

      fields = r.fields.toList.map(f => createField(f))

      body = Doc.intercalate(fieldDelim, fields)

      record = body.tightBracketBy(
        caseClassPrefix(typ),
        caseClassPostfix,
        indent
      )

      features <- features.collectFeatures(_.handleRecord(r))

      result = features.toNel.fold(record) { f =>
        val prefix = objectPrefix(createTypeExpr(r.name.name, Nil))
        val companion = Doc
          .intercalate(dblLine, f.toList)
          .tightBracketBy(prefix, objectPostfix, indent)

        Doc.intercalate(dblLine, List(record, companion))
      }

    } yield result

}
