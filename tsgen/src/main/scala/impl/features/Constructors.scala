package sculptor.tsgen
package impl
package features

import org.typelevel.paiges._

import sculptor.ast._

object Constructors extends Feature with GenHelpers {

  private def genConstructor(opt: Option[OptionalEncoding])(
    name: Ident,
    typ: Doc,
    genParams: List[GenericDef],
    params: List[FieldDef],
    indent: Int
  )(body: => Doc): Doc = {
    val prefix = functionPrefix(opt)(name.name, genParams, params, typ) + Doc
      .text(" {")
    exported(body.tightBracketBy(prefix, functionPostfix, indent))
  }

  override def handleNewtype(n: Newtype) =
    for {
      indent <- getIndent
      opt <- getOptionalEncoding
      typ = createTypeRef(n.ref)
    } yield
      List(
        genConstructor(opt)(
          n.name,
          typ,
          n.parameters,
          List(FieldDef(Ident("value"), n.baseType)),
          indent
        ) {
          Doc.text("return value as ") + typ
        }
      )

  private def genObjectConstructor(opt: Option[OptionalEncoding])(
    name: Ident,
    typ: Doc,
    genParams: List[GenericDef],
    paramsPrefixes: List[Doc],
    params: List[FieldDef],
    indent: Int
  ): Doc =
    genConstructor(opt)(name, typ, genParams, params, indent) {
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
      opt <- getOptionalEncoding
      typ = createTypeRef(r.ref)
    } yield
      List(
        genObjectConstructor(opt)(
          r.name,
          typ,
          r.parameters,
          Nil,
          r.fields.toList,
          indent
        )
      )

  private def genADTConstructor(a: ADT,
                                tagName: String,
                                opt: Option[OptionalEncoding],
                                genAdtNs: Boolean,
                                indent: Int)(c: ADTConstructor): Doc = {
    val typ = createTypeRef(a.ref)
    val tag = Doc.text(c.name.name)
    val adtTag = Doc.text(s"""$tagName: """") + tag + Doc.char('"')
    genObjectConstructor(opt)(
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
      opt <- getOptionalEncoding
      tagName <- getAdtTag
      genAdtNs <- getGenerateAdtNs
      constructors0 = a.constructors.toList
        .map(genADTConstructor(a, tagName, opt, genAdtNs, indent))
      constructors = genAdtNs match {
        case false => constructors0
        case _ => List(withNamespace(a.name.name, indent)(constructors0))
      }
    } yield constructors

}
