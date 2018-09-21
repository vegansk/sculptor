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
      .map(Doc.intercalate(dblLine, _))

  def generate(r: ADT): Result[Doc] =
    for {

      indent <- getIndent

      typ = createTypeExpr(r.name.name, r.parameters.map(createGenericParam))

      objType = createTypeExpr(r.name.name, Nil)

      trait_ = sealedTrait(typ)

      implPrefix = objectPrefix(objType)

      implBody <- generateImplBody(r, typ, indent)

      impl = implBody.tightBracketBy(implPrefix, objectPostfix, indent)

    } yield Doc.intercalate(dblLine, List(trait_, impl))

}
