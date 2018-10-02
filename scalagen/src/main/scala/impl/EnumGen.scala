package sculptor.scalagen.impl

import cats.implicits._
import org.typelevel.paiges._

import sculptor.ast._

object EnumGen extends GenHelpers {

  def generateEnumValue(e: EnumValue, enumType: Doc): Result[Doc] =
    ok(extend(caseObject(Doc.text(e.name.name)), enumType))

  def generateAsString(e: Enum, enumType: Doc, indent: Int): Result[Doc] = {
    val prefix = Doc.text("val asString: ") + enumType + Doc.text(
      " => String = {"
    )
    val postfix = objectPostfix

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
    val postfix = objectPostfix

    val cases = e.values.map { v =>
      Doc.text(s"""case "${v.value}" => ${v.name.name}""")
    }

    ok(
      Doc
        .intercalate(line, cases.toList)
        .tightBracketBy(prefix, postfix, indent)
    )
  }

  def generateEnumBody(e: Enum, enumType: Doc, indent: Int): Result[Doc] =
    for {
      values <- e.values.toList
        .traverse(generateEnumValue(_, enumType))
        .map(Doc.intercalate(line, _))

      asString <- generateAsString(e, enumType, indent)

      fromString <- generateFromString(e, enumType, indent)

    } yield Doc.intercalate(dblLine, values :: asString :: fromString :: Nil)

  def generate(e: Enum): Result[Doc] =
    for {
      indent <- getIndent

      typ = createTypeExpr(e.name.name, Nil)

      trait_ = adtSealedTrait(typ)

      enumPrefix = objectPrefix(typ)

      body <- generateEnumBody(e, typ, indent)

      features <- features.collectFeatures(_.handleEnum(e))

      enum_ = Doc
        .intercalate(dblLine, body :: features)
        .tightBracketBy(enumPrefix, objectPostfix, indent)

    } yield Doc.intercalate(dblLine, List(trait_, enum_))

}
