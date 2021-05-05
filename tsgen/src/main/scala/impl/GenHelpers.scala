package sculptor.tsgen
package impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._
import sculptor.common._

trait GenHelpers extends CommonGenHelpers {

  def createTypeRef(r: TypeRef, optional: Boolean = false): Doc = {
    def generic(g: TypeRef.Generic) =
      createTypeExpr(g.name.name, Nil)

    def specialized(s: TypeRef.Specialized) =
      createTypeExpr0(
        s.name.mkString("."),
        s.parameters.map(createTypeRef(_, false))
      )

    TypeRef.cata(specialized, generic)(r) + (
      if (optional)
        Doc.text(" | undefined")
      else
        Doc.empty
    )
  }

  def createGenericParam(g: GenericDef): Doc =
    Doc.text(g.`type`.name.name) + g.`extends`.toNel
      .map(
        l =>
          Doc.text(" extends ") + Doc
            .intercalate(Doc.text(" & "), l.toList.map(createTypeRef(_, false)))
      )
      .getOrElse(Doc.empty)

  def createParameters0(l0: List[Doc], paramPostfix: Doc = Doc.empty): Doc = {
    l0.toNel
      .map { l =>
        Doc.char('<') + Doc.intercalate(
          Doc.text(", "),
          l.toList.map(_ + paramPostfix)
        ) + Doc
          .char('>')
      }
      .getOrElse(Doc.empty)
  }

  def createParameters(l0: List[GenericDef]): Doc =
    createParameters0(l0.map(p => Doc.text(p.`type`.name.name)))

  def createTypeExpr0(name: String, parameters: List[Doc]): Doc =
    Doc.text(name) + createParameters0(parameters)

  def createTypeExpr(name: String, parameters: List[GenericDef]): Doc =
    Doc.text(name) + createParameters(parameters)

  def createField0(name: Ident, `type`: TypeRef): Doc =
    Doc.text(name.name) + Doc.text(": ") + createTypeRef(`type`)

  def createFuncParam(opt: Option[OptionalEncoding])(f0: FieldDef): Doc = {
    val (optional, f) = processField(opt)(f0)
    createField1(f.name, createTypeRef(f.`type`, optional), false)
  }

  def createField1(name: Ident, `type`: Doc, optional: Boolean): Doc =
    Doc.text(name.name) + (if (optional) Doc.char('?') else Doc.empty) + Doc
      .text(": ") + `type`

  def processField(
    optionalEncoding: Option[OptionalEncoding]
  )(f: FieldDef): (Boolean, FieldDef) = {
    val (o, r) = optionalEncoding.fold((false, f.`type`)) { e =>
      TypeRef.cata(
        s =>
          s.parameters.headOption.fold((false, s: TypeRef)) { p =>
            if (s.name.mkString(".") === e.optionalType) (true, p)
            else (false, s)
        },
        _ => (false, f.`type`)
      )(f.`type`)
    }
    (
      optionalEncoding.map(_.allFieldsOptional).getOrElse(false) || o,
      f.copy(`type` = r)
    )
  }

  def createField(
    withComment: Boolean
  )(f0: FieldDef, optionalEncoding: Option[OptionalEncoding]): Doc = {
    val (optional, f) = processField(optionalEncoding)(f0)
    createField1(f.name, createTypeRef(f.`type`), optional) + optionalComment(
      withComment
    )(f.comment).map(spacePrefix).getOrElse(Doc.empty)
  }

  def createEnumValue(withComment: Boolean)(v: EnumValue): Doc = {
    Doc.text(s"""${v.name.name} = "${v.value}"""") + optionalComment(
      withComment
    )(v.comment).map(spacePrefix).getOrElse(Doc.empty)
  }

  def interfacePrefix(`type`: Doc): Doc =
    Doc.text("interface ") + `type` + Doc.text(" {")

  val newtypePostfix = Doc.char('}')

  val functionPostfix = Doc.char('}')

  val interfacePostfix = Doc.char('}')

  val fieldDelim = Doc.char(',') + Doc.line

  val line = Doc.hardLine

  val dblLine = Doc.hardLine * 2

  def exported(what: Doc): Doc =
    Doc.text("export ") + what

  def functionPrefix(opt: Option[OptionalEncoding], generateField: Boolean)(
    name: String,
    genParams: List[GenericDef],
    params: List[FieldDef],
    resultType: Doc
  ): Doc = {
    val prefix =
      if (generateField)
        Doc.text(s"${name}: ") + createParameters(genParams) + Doc
          .char('(')
      else
        Doc.text(s"const ${name} = ") + createParameters(genParams) + Doc
          .char('(')
    val postfix = Doc.text("): ") + resultType + Doc.text(" =>")

    prefix + Doc.intercalate(Doc.text(", "), params.map(createFuncParam(opt))) + postfix
  }

  def createBrandField(typ: Doc): Doc =
    Doc.text("""__brand: """") + typ + Doc.char('"')

  def beginNamespace(ns: String): Doc =
    exported(Doc.text(s"declare namespace $ns {"))

  val endNamespace = Doc.char('}')

  def withNamespace(ns: String, indent: Int)(what: List[Doc]): Doc =
    bracketBy(Doc.intercalate(dblLine, what))(
      beginNamespace(ns),
      endNamespace,
      indent
    )
}
