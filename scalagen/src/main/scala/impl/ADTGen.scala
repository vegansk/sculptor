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

  def generateImplBody(r: ADT, adtType: Doc, indent: Int): Result[Doc] =
    r.constructors.toList
      .traverse(generateConstructor(_, adtType, indent))
      .map(_.flatten)
      .map(Doc.intercalate(line, _))

  def generate(a: ADT): Result[Doc] =
    for {

      indent <- getIndent

      genComments <- getGenerateComments

      typ = createTypeExpr(a.name.name, a.parameters)

      comment = Option(genComments)
        .filter(identity)
        .map(_ => typeComment(a, typ))

      objType = createTypeExpr(a.name.name, Nil)

      trait_ = adtSealedTrait(typ)

      implPrefix = objectPrefix(objType)

      implBody <- generateImplBody(a, typ, indent)

      features <- features.collectFeatures(_.handleADT(a))

      impl = Doc
        .intercalate(dblLine, implBody :: features)
        .tightBracketBy(implPrefix, objectPostfix, indent)

    } yield Doc.intercalate(dblLine, comment.toList ++ List(trait_, impl))

}
