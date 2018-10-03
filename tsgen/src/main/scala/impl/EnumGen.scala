package sculptor.tsgen.impl

import org.typelevel.paiges._

import sculptor.ast._

object EnumGen extends GenHelpers {

  private def generateEnumBody(e: Enum, indent: Int) =
    Doc.intercalate(Doc.char(',') + line, e.values.toList.map { v =>
      Doc.text(s"""${v.name.name} = "${v.value}"""")
    })

  def generate(e: Enum): Result[Doc] =
    for {
      indent <- getIndent

      prefix = Doc.text(s"enum ${e.name.name} {")
      postfix = interfacePostfix

      body = generateEnumBody(e, indent)

      enum_ = exported(body.tightBracketBy(prefix, postfix, indent))

      features <- features.collectFeatures(_.handleEnum(e))

    } yield Doc.intercalate(dblLine, enum_ :: features)

}
