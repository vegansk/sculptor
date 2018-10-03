package sculptor.tsgen
package impl
package features

import org.typelevel.paiges._

import sculptor.ast._

object Constructors extends Feature with GenHelpers {

  private def genConstructor(name: Ident,
                             typ: Doc,
                             genParams: List[GenericDef],
                             params: List[FieldDef],
                             indent: Int)(body: => Doc): Doc = {
    val prefix = functionPrefix(name.name, genParams, params, typ) + Doc.text(
      " {"
    )
    body.tightBracketBy(prefix, objectPostfix, indent)
  }

  override def handleNewtype(n: Newtype) =
    for {
      indent <- getIndent
      typ = createTypeRef(n.ref)
    } yield
      List(
        genConstructor(
          n.name,
          typ,
          n.parameters,
          List(FieldDef(Ident("value"), n.baseType)),
          indent
        ) {
          Doc.text("return value as ") + typ
        }
      )

  private def genObjectConstructor(name: Ident,
                                   typ: Doc,
                                   genParams: List[GenericDef],
                                   paramsPrefixes: List[Doc],
                                   params: List[FieldDef],
                                   indent: Int): Doc =
    genConstructor(name, typ, genParams, params, indent) {
      Doc
        .intercalate(
          Doc.char(',') + line,
          paramsPrefixes ++ params.map(f => Doc.text(f.name.name))
        )
        .tightBracketBy(Doc.text("return {"), Doc.char('}'), indent)
    }

  override def handleRecord(r: Record) =
    for {
      indent <- getIndent
      typ = createTypeRef(r.ref)
    } yield
      List(
        genObjectConstructor(
          r.name,
          typ,
          r.parameters,
          Nil,
          r.fields.toList,
          indent
        )
      )

  private def genADTConstructor(a: ADT,
                                c: ADTConstructor,
                                tagName: String,
                                indent: Int): Doc = {
    val typ = createTypeRef(a.ref)
    val consTyp = createTypeExpr(c.name.name, c.parameters)
    val adtTag = Doc.text(s"""$tagName: """") + consTyp + Doc.char('"')
    genObjectConstructor(
      c.name,
      typ,
      c.parameters,
      List(adtTag),
      c.fields,
      indent
    )
  }

  override def handleADT(a: ADT) =
    for {
      indent <- getIndent
      tagName <- getAdtTag
    } yield a.constructors.toList.map(genADTConstructor(a, _, tagName, indent))

}
