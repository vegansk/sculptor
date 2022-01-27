package sculptor.scalagen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._

object ADTGen extends GenHelpers {

  import ScalaIdent._

  def generateConstructor(c: ADTConstructor,
                          adtType: Doc,
                          indent: Int): Result[List[Doc]] =
    for {
      genComments <- getGenerateComments
      comment = optionalComment(genComments)(c.comment)
      typ = createTypeExpr(c.name, c.parameters)
      typeDef = {
        if (c.fields.isEmpty && c.parameters.isEmpty)
          caseObject0(typ)
        else if (c.fields.isEmpty)
          caseClassPrefix(typ) + caseClassPostfix
        else
          bracketBy(
            Doc
              .intercalate(fieldDelim, c.fields.map(createField(genComments)))
          )(caseClassPrefix(typ), caseClassPostfix, indent)
      }

    } yield comment.toList ++ List(extend(typeDef, adtType))

  def generateImplBody(a: ADT, adtType: Doc, indent: Int): Result[Doc] =
    a.constructors.toList
      .traverse(generateConstructor(_, adtType, indent))
      .map(_.flatten)
      .map(Doc.intercalate(line, _))

  def generateHelper(c: ADTConstructor, adtType: Doc, indent: Int): Doc = {
    val name = decapitalize(c.name.asScalaId)
    val fName = createTypeExpr0(name, c.parameters)
    val fImpl = createTypeExpr(c.name, c.parameters)
    (c.parameters, c.fields) match {
      case (Nil, Nil) =>
        Doc.text("val ") + fName + Doc.text(": ") + adtType + Doc.text(" = ") + fImpl
      case (_, Nil) =>
        Doc.text("def ") + fName + Doc.text(": ") + adtType + Doc.text(" = ") + fImpl + Doc
          .text("()")
      case (_, fields) => {
        val func = bracketBy(
          Doc
            .intercalate(fieldDelim, c.fields.map(createField(false)))
        )(fName + Doc.char('('), Doc.char(')'), indent)
        val call = bracketBy(
          Doc
            .intercalate(
              paramDelim,
              fields.map(f => Doc.text(f.name.asScalaId))
            )
        )(fImpl + Doc.char('('), Doc.char(')'), indent)
        Doc.text("def ") + func + Doc.text(": ") + adtType + Doc.text(" = ") + call
      }
    }
  }

  def generateHelpers(a: ADT, adtType: Doc, indent: Int): Doc =
    Doc.intercalate(
      line,
      a.constructors.toList
        .map(generateHelper(_, adtType, indent))
    )

  def generate(a: ADT): Result[Doc] =
    for {

      indent <- getIndent

      genComments <- getGenerateComments

      genHelpers <- getGenerateAdtConstructorsHelpers

      typ = createTypeExpr(a.name, a.parameters)

      comment = Option(genComments)
        .filter(identity)
        .map(_ => typeComment(a, typ))

      trait_ = adtSealedTrait0(typ)

      implPrefix = objectPrefix(a.name)

      implBody <- generateImplBody(a, typ, indent)

      helpers = genHelpers match {
        case false => List[Doc]()
        case _ => List(generateHelpers(a, typ, indent))
      }

      features <- features.collectFeatures(_.handleADT(a))

      impl = bracketBy(
        Doc
          .intercalate(dblLine, List(implBody) ++ helpers ++ features)
      )(implPrefix, objectPostfix, indent)

    } yield Doc.intercalate(dblLine, comment.toList ++ List(trait_, impl))

}
