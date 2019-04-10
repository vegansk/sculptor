package sculptor.scalagen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._

object ADTGen extends GenHelpers {

  def generateConstructor(c: ADTConstructor,
                          adtType: Doc,
                          indent: Int): Result[List[Doc]] =
    for {
      genComments <- getGenerateComments
      comment = optionalComment(genComments)(c.comment)
      typ = createTypeExpr(c.name.name, c.parameters)
      typeDef = {
        if (c.fields.isEmpty && c.parameters.isEmpty)
          caseObject(typ)
        else
          Doc
            .intercalate(fieldDelim, c.fields.map(createField(genComments)))
            .tightBracketBy(caseClassPrefix(typ), caseClassPostfix, indent)
      }

    } yield comment.toList ++ List(extend(typeDef, adtType))

  def generateImplBody(a: ADT, adtType: Doc, indent: Int): Result[Doc] =
    a.constructors.toList
      .traverse(generateConstructor(_, adtType, indent))
      .map(_.flatten)
      .map(Doc.intercalate(line, _))

  def generateHelper(c: ADTConstructor, adtType: Doc, indent: Int): Doc = {
    val name = decapitalize(c.name.name)
    val fName = createTypeExpr(name, c.parameters)
    val fImpl = createTypeExpr(c.name.name, c.parameters)
    (c.parameters, c.fields) match {
      case (Nil, Nil) =>
        Doc.text("val ") + fName + Doc.text(": ") + adtType + Doc.text(" = ") + fImpl
      case (_, Nil) =>
        Doc.text("def ") + fName + Doc.text(": ") + adtType + Doc.text(" = ") + fImpl + Doc
          .text("()")
      case (_, fields) => {
        val func = Doc
          .intercalate(fieldDelim, c.fields.map(createField(false)))
          .tightBracketBy(fName + Doc.char('('), Doc.char(')'), indent)
        val call = Doc
          .intercalate(paramDelim, fields.map(f => Doc.text(f.name.name)))
          .tightBracketBy(fImpl + Doc.char('('), Doc.char(')'), indent)
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

      typ = createTypeExpr(a.name.name, a.parameters)

      comment = Option(genComments)
        .filter(identity)
        .map(_ => typeComment(a, typ))

      objType = createTypeExpr(a.name.name, Nil)

      trait_ = adtSealedTrait(typ)

      implPrefix = objectPrefix(objType)

      implBody <- generateImplBody(a, typ, indent)

      helpers = genHelpers match {
        case false => List[Doc]()
        case _ => List(generateHelpers(a, typ, indent))
      }

      features <- features.collectFeatures(_.handleADT(a))

      impl = Doc
        .intercalate(dblLine, List(implBody) ++ helpers ++ features)
        .tightBracketBy(implPrefix, objectPostfix, indent)

    } yield Doc.intercalate(dblLine, comment.toList ++ List(trait_, impl))

}
