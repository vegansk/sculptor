package sculptor.scalagen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._

object ADTGen extends GenHelpers {

  def generateConstructor(c: ADTConstructor,
                          adtType: Doc,
                          indent: Int): Result[Doc] =
    for {
      typ <- ok(
        createTypeExpr(c.name.name, c.parameters.map(createGenericParam _))
      )
      typeDef = {
        if (c.fields.isEmpty && c.parameters.isEmpty)
          caseObject(typ)
        else
          Doc
            .intercalate(fieldDelim, c.fields.map(createField))
            .tightBracketBy(caseClassPrefix(typ), caseClassPostfix, indent)
      }

    } yield extend(typeDef, adtType)

  def generateImplBody(r: ADT, adtType: Doc, indent: Int): Result[Doc] =
    r.constructors.toList
      .traverse(generateConstructor(_, adtType, indent))
      .map(Doc.intercalate(line, _))

  def generate(a: ADT): Result[Doc] =
    for {

      indent <- getIndent

      typ = createTypeExpr(a.name.name, a.parameters.map(createGenericParam))

      objType = createTypeExpr(a.name.name, Nil)

      trait_ = adtSealedTrait(typ)

      implPrefix = objectPrefix(objType)

      implBody <- generateImplBody(a, typ, indent)

      features <- features.collectFeatures(_.handleADT(a))

      impl = Doc
        .intercalate(dblLine, implBody :: features)
        .tightBracketBy(implPrefix, objectPostfix, indent)

    } yield Doc.intercalate(dblLine, List(trait_, impl))

}
