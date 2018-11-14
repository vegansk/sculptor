package sculptor.tsgen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._

object ADTGen extends GenHelpers {

  private def generateConstructor(c: ADTConstructor, indent: Int): Result[Doc] =
    for {
      genComment <- getGenerateComments
      comment = optionalComment(genComment)(c.comment)
      adtTag <- getAdtTag
      optEnc <- getOptionalEncoding
      typ = createTypeExpr(c.name.name, c.parameters)
      result = Doc.intercalate(
        line,
        comment.toList ++
          List(
            Doc
              .intercalate(
                line,
                Doc.text(s"""$adtTag: """") + typ + Doc.char('"') ::
                  c.fields.map(createField(genComment)(_, optEnc))
              )
              .tightBracketBy(
                exported(interfacePrefix(typ)),
                interfacePostfix,
                indent
              )
          )
      )
    } yield result

  def generate(a: ADT): Result[Doc] =
    for {

      indent <- getIndent

      typ = createTypeExpr(a.name.name, a.parameters)

      genComment <- getGenerateComments

      comment = Option(genComment)
        .filter(identity)
        .map(_ => typeComment(a, typ))

      consList = Doc.intercalate(
        Doc.text(" | "),
        a.constructors.toList
          .map(c => createTypeExpr(c.name.name, c.parameters))
      )

      adtType = exported(Doc.text("type ") + typ + Doc.text(" = ") + consList)

      constructors <- a.constructors.toList
        .traverse(generateConstructor(_, indent))

      features <- features.collectFeatures(_.handleADT(a))

    } yield
      Doc.intercalate(
        dblLine,
        comment.toList ++ List(adtType) ++ constructors ++ features
      )

}
