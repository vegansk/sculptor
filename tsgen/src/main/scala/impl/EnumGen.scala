package sculptor.tsgen.impl

import org.typelevel.paiges._

import sculptor.ast._

object EnumGen extends GenHelpers {

  def generate(e: Enum): Result[Doc] =
    for {
      indent <- getIndent

      typ = createTypeExpr(e.name.name, Nil)

      genComment <- getGenerateComments

      comment = Option(genComment)
        .filter(identity)
        .map(_ => typeComment(e, typ))

      prefix = exported(Doc.text("enum ") + typ + Doc.text(" {"))
      postfix = interfacePostfix

      body = Doc.intercalate(
        Doc.char(',') + line,
        e.values.toList.map(createEnumValue(genComment))
      )

      enum_ = body.tightBracketBy(prefix, postfix, indent)

      features <- features.collectFeatures(_.handleEnum(e))

    } yield Doc.intercalate(dblLine, comment.toList ++ List(enum_) ++ features)

}
