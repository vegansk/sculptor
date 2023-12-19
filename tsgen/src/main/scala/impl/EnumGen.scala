package sculptor.tsgen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._

object EnumGen extends GenHelpers {

  def generateValues(e: Enum, enumType: Doc, indent: Int): Result[Doc] = {
    val prefix = exported(
      Doc.text(s"const ${decapitalize(e.name.name)}Values: ") + enumType + Doc
        .text("[] = [")
    )
    val postfix = Doc.char(']')

    ok(
      bracketBy(
        Doc
          .intercalate(
            Doc.comma + Doc.lineOrSpace,
            e.values.toList.map(v => Doc.text(s"${e.name.name}.${v.name.name}"))
          )
      )(prefix, postfix, indent)
    )
  }

  def generateDescriptions(e: Enum,
                           enumType: Doc,
                           indent: Int): Result[Option[Doc]] =
    e.values.toList
      .map(v => v.comment.map((v, _)))
      .flattenOption
      .toNel
      .fold[Result[Option[Doc]]](ok(None)) { valsWithDesc =>
        val funType = if (valsWithDesc.length === e.values.length) {
          Doc.text("string")
        } else {
          Doc.text("string | undefined")
        }
        val prefix = exported(
          Doc.text(s"const ${decapitalize(e.name.name)}Description = (v: ") + enumType + Doc
            .text("): ")
            + funType + Doc.text(" => {")
        )
        val postfix = functionPostfix
        val cases = bracketBy(
          Doc
            .intercalate(line, valsWithDesc.toList.map {
              case (v, d) =>
                Doc.text(s"""case ${e.name.name}.${v.name.name}: return "$d"""")
            })
        )(Doc.text("switch(v) {"), Doc.char('}'), indent)
        ok(bracketBy(cases)(prefix, postfix, indent).some)
      }

  def generate(e: Enum): Result[Doc] =
    for {
      indent <- getIndent

      typ = createTypeExpr(e.name.name, Nil)

      genComment <- getGenerateComments
      genDescriptions <- getGenerateEnumsDescriptions

      comment = Option(genComment)
        .filter(identity)
        .map(_ => typeComment(e, typ))

      prefix = exported(Doc.text("enum ") + typ + Doc.text(" {"))
      postfix = interfacePostfix

      body = Doc.intercalate(
        Doc.char(',') + line,
        e.values.toList.map(createEnumValue(genComment))
      )

      enum_ = bracketBy(body)(prefix, postfix, indent)

      values <- generateValues(e, typ, indent)

      descriptions <- if (genDescriptions) generateDescriptions(e, typ, indent)
      else ok(None)

      features <- features.collectFeatures(_.handleEnum(e))

    } yield
      Doc.stack(
        comment.toList ++ (
          Doc.intercalate(
            dblLine,
            List(enum_, values) ++ descriptions.toList ++ features
          ) :: Nil
        )
      )

}
