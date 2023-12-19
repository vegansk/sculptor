package sculptor.scalagen.impl

import cats.implicits._
import org.typelevel.paiges._

import sculptor.ast._

object EnumGen extends GenHelpers {

  import ScalaIdent._

  def generateEnumValue(genComment: Boolean)(e: EnumValue,
                                             enumType: Doc): List[Doc] =
    List(
      optionalComment(genComment)(e.comment),
      extend(caseObject(e.name), enumType).some
    ).flattenOption

  def generateValues(e: Enum, enumType: Doc, indent: Int): Result[Doc] = {
    val prefix = Doc.text("val values: List[") + enumType + Doc.text(
      "] = List("
    )
    val postfix = Doc.char(')')

    ok(
      bracketBy(
        Doc
          .intercalate(
            Doc.comma + Doc.lineOrSpace,
            e.values.toList.map(v => Doc.text(v.name.asScalaId))
          )
      )(prefix, postfix, indent)
    )
  }

  def generateAsString(e: Enum, enumType: Doc, indent: Int): Result[Doc] = {
    val prefix = Doc.text("val asString: ") + enumType + Doc.text(
      " => String = {"
    )
    val postfix = functionPostfix

    val cases = e.values.map { v =>
      Doc.text(s"""case ${v.name.asScalaId} => "${v.value}"""")
    }

    ok(
      bracketBy(
        Doc
          .intercalate(line, cases.toList)
      )(prefix, postfix, indent)
    )
  }

  def generateFromString(e: Enum, enumType: Doc, indent: Int): Result[Doc] = {
    val prefix = Doc.text("val fromString: PartialFunction[String, ") + enumType + Doc
      .text("] = {")
    val postfix = functionPostfix

    val cases = e.values.map { v =>
      Doc.text(s"""case "${v.value}" => ${v.name.asScalaId}""")
    }

    ok(
      bracketBy(
        Doc
          .intercalate(line, cases.toList)
      )(prefix, postfix, indent)
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
          bracketBy(
            Doc
              .intercalate(line, cases.toList)
          )(prefix, postfix, indent).some
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

      typ = createTypeExpr(e.name, Nil)

      comment <- getGenerateComments.map(doc(_)(typeComment(e, typ)))

      trait_ = adtSealedTrait(e.name)

      enumPrefix = objectPrefix(e.name)

      body <- generateEnumBody(e, typ, indent)

      features <- features.collectFeatures(_.handleEnum(e))

      enum_ = bracketBy(
        Doc
          .intercalate(dblLine, body :: features)
      )(enumPrefix, objectPostfix, indent)

    } yield
      Doc.stack(
        comment.toList ++ (
          Doc.intercalate(dblLine, List(trait_, enum_)) :: Nil
        )
      )

}
