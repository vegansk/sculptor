package sculptor.scalagen.impl

import cats.implicits._
import org.typelevel.paiges._

import sculptor.ast._

object EnumGen extends GenHelpers {

  def generateEnumValue(genComment: Boolean)(e: EnumValue,
                                             enumType: Doc): List[Doc] =
    Option(genComment)
      .filter(identity)
      .flatMap(_ => e.comment)
      .map(c => Doc.text(s"// $c"))
      .toList ++
      List(extend(caseObject(Doc.text(e.name.name)), enumType))

  def generateValues(e: Enum, enumType: Doc, indent: Int): Result[Doc] = {
    val prefix = Doc.text("val values: List[") + enumType + Doc.text(
      "] = List("
    )
    val postfix = Doc.char(')')

    ok(
      Doc
        .intercalate(
          Doc.comma + Doc.lineOrSpace,
          e.values.toList.map(v => Doc.text(v.name.name))
        )
        .tightBracketBy(prefix, postfix, indent)
    )
  }

  def generateAsString(e: Enum, enumType: Doc, indent: Int): Result[Doc] = {
    val prefix = Doc.text("val asString: ") + enumType + Doc.text(
      " => String = {"
    )
    val postfix = functionPostfix

    val cases = e.values.map { v =>
      Doc.text(s"""case ${v.name.name} => "${v.value}"""")
    }

    ok(
      Doc
        .intercalate(line, cases.toList)
        .tightBracketBy(prefix, postfix, indent)
    )
  }

  def generateFromString(e: Enum, enumType: Doc, indent: Int): Result[Doc] = {
    val prefix = Doc.text("val fromString: PartialFunction[String, ") + enumType + Doc
      .text("] = {")
    val postfix = functionPostfix

    val cases = e.values.map { v =>
      Doc.text(s"""case "${v.value}" => ${v.name.name}""")
    }

    ok(
      Doc
        .intercalate(line, cases.toList)
        .tightBracketBy(prefix, postfix, indent)
    )
  }

  def generateDescription(e: Enum,
                          enumType: Doc,
                          indent: Int): Result[Option[Doc]] =
    e.values.toList
      .map(v => v.comment.map((v, _)))
      .flattenOption
      .toNel
      .fold[Result[Option[Doc]]](ok(None)) { valsWithDesc =>
        val funType = if (valsWithDesc.length === e.values.length) {
          enumType + Doc.text(" => String")
        } else {
          Doc.text("PartialFunction[") + enumType + Doc.text(", String]")
        }
        val prefix = Doc.text("val description: ") + funType + Doc.text(" = {")
        val postfix = functionPostfix

        val cases = valsWithDesc.map {
          case (v, d) =>
            Doc.text(s"""case ${v.name.name} => "$d"""")
        }

        ok(
          Doc
            .intercalate(line, cases.toList)
            .tightBracketBy(prefix, postfix, indent)
            .some
        )
      }

  def generateEnumBody(e: Enum, enumType: Doc, indent: Int): Result[Doc] =
    for {
      genComments <- getGenerateComments
      genDescriptions <- getGenerateEnumDescriptions
      values = Doc.intercalate(
        line,
        e.values.toList
          .map(generateEnumValue(genComments)(_, enumType))
          .combineAll
      )

      valuesList <- generateValues(e, enumType, indent)

      asString <- generateAsString(e, enumType, indent)

      fromString <- generateFromString(e, enumType, indent)

      descriptions <- if (genDescriptions)
        generateDescription(e, enumType, indent)
      else ok(None)

    } yield
      Doc.intercalate(
        dblLine,
        values :: valuesList :: asString :: fromString :: descriptions.toList
      )

  def generate(e: Enum): Result[Doc] =
    for {
      indent <- getIndent

      typ = createTypeExpr(e.name.name, Nil)

      comment <- getGenerateComments.map(doc(_)(typeComment(e, typ)))

      trait_ = adtSealedTrait(typ)

      enumPrefix = objectPrefix(typ)

      body <- generateEnumBody(e, typ, indent)

      features <- features.collectFeatures(_.handleEnum(e))

      enum_ = Doc
        .intercalate(dblLine, body :: features)
        .tightBracketBy(enumPrefix, objectPostfix, indent)

    } yield Doc.intercalate(dblLine, comment.toList ++ List(trait_, enum_))

}
