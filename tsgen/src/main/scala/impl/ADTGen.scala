package sculptor.tsgen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._

object ADTGen extends GenHelpers {

  private def generateConstructor(c: ADTConstructor, indent: Int): Result[Doc] =
    for {
      adtTag <- getAdtTag
      optEnc <- getOptionalEncoding
      typ = createTypeExpr(c.name.name, c.parameters)
      result = Doc
        .intercalate(
          line,
          Doc.text(s"""$adtTag: """") + typ + Doc.char('"') ::
            c.fields.map(createField(_, optEnc))
        )
        .tightBracketBy(
          exported(interfacePrefix(typ)),
          interfacePostfix,
          indent
        )
    } yield result

  def generate(a: ADT): Result[Doc] =
    for {

      indent <- getIndent

      typ = createTypeExpr(a.name.name, a.parameters)

      consList = Doc.intercalate(
        Doc.text(" | "),
        a.constructors.toList
          .map(c => createTypeExpr(c.name.name, c.parameters))
      )

      adtType = exported(Doc.text("type ") + typ + Doc.text(" = ") + consList)

      constructors <- a.constructors.toList
        .traverse(generateConstructor(_, indent))

      features <- features.collectFeatures(_.handleADT(a))

    } yield Doc.intercalate(dblLine, adtType :: constructors ++ features)

}
