package sculptor.tsgen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._

object ADTGen extends GenHelpers {

  private def generateConstructor(a: ADT)(c: ADTConstructor): Result[Doc] =
    for {
      indent <- getIndent
      genComment <- getGenerateComments
      comment = optionalComment(genComment)(c.comment)
      adtTag <- getAdtTag
      optEnc <- getOptionalEncoding
      genAdtNs <- getGenerateAdtNs
      typ = createTypeExpr0(c.name.name, c.parameters.map(createGenericParam))
      tag = Doc.text(c.name.name)
      result = Doc.intercalate(
        line,
        comment.toList ++
          List(
            Doc
              .intercalate(
                line,
                Doc.text(s"""$adtTag: """") + tag + Doc.char('"') ::
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

      typ = createTypeExpr0(a.name.name, a.parameters.map(createGenericParam))

      genComment <- getGenerateComments

      genAdtNs <- getGenerateAdtNs

      comment = Option(genComment)
        .filter(identity)
        .map(_ => typeComment(a, typ))

      consList = Doc.intercalate(
        Doc.text(" | "),
        a.constructors.toList
          .map { c =>
            val name =
              if (genAdtNs) s"${a.name.name}.${c.name.name}" else c.name.name
            createTypeExpr(name, c.parameters)
          }
      )

      adtType = exported(Doc.text("type ") + typ + Doc.text(" = ") + consList)

      constructors0 <- a.constructors.toList
        .traverse(generateConstructor(a))

      constructors = genAdtNs match {
        case false => constructors0
        case _ => List(withNamespace(a.name.name, indent)(constructors0))
      }

      features <- features.collectFeatures(_.handleADT(a))

    } yield
      Doc.intercalate(
        dblLine,
        comment.toList ++ List(adtType) ++ constructors ++ features
      )

}
