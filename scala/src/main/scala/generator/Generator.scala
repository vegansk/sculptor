package sculptor
package scala
package generator

import cats._
import cats.implicits._
import org.typelevel.paiges._

class Generator(config: Config) {

  private def tabSize = 2

  import ast._

  import Doc._

  val dot: Doc = char('.')

  val eqSign: Doc = char('=')

  val asterisk: Doc = char('*')

  val const: Doc = text("const")

  val `type`: Doc = text("type")

  val `import`: Doc = text("import")

  def strLit(v: String): Doc = char('"') + text(v) + char('"')

  def ident(i: Ident): Doc = text(i.value)

  def bracketBy(d: Doc)(left: Doc, right: Doc): Doc =
    left + (lineBreak + d).nested(tabSize) + lineBreak + right

  def qName(name: QName): Doc =
    intercalate(dot, name.path.map(ident).toList)

  def array(`type`: Doc): Doc =
    text("List[") + `type` + char(']')

  def comment(c: Option[Comment]): List[Doc] =
    c.filter(_ => config.generateComments)
      .map(_.replace("\n", " "))
      .fold[List[Doc]](Nil)(t => List(text("/** ") + text(t) + text(" */")))

  def nativeArray(`type`: Doc): Doc =
    `type` + text("[]")

  def optional(`type`: Doc): Doc =
    text("Option[") + `type` + char(']')

  private val optionalConstraints =
    List(
      FieldConstraint.Nullable,
      FieldConstraint.OptionalNullable,
      FieldConstraint.Optional
    )

  def nativeTypeExpr(f: FieldDecl): Doc = {
    (typeName(f.`type`): Id[Doc])
      .map(t => if (f.array) nativeArray(t) else t)
  }

  def complexTypeDecl(ct: ComplexTypeDecl): Doc = {
    val base = ct.baseType.fold(List[Doc]()) { bt =>
      List(text("extends"), typeName(bt))
    }
    val prefix = spread(
      List(text("final case class"), ident(ct.`type`.name)) ++ base
    ) + char('(')
    val postfix = char(')')
    stack(
      comment(ct.comment) ++
        bracketBy(intercalate(comma + line, ct.fields.toList.map(fieldDecl _)))(
          prefix,
          postfix
        ).pure[List]
    )
  }

  def typeName(t: TypeRef): Doc = t match {
    case TypeRef.std(v) => ident(v)
    case TypeRef.defined(v) => ident(v)
    case TypeRef.external(v) => qName(v)
  }

  def typeExpr(f: FieldDecl): Doc = {
    (typeName(f.`type`): Id[Doc])
      .map(t => if (f.array) array(t) else t)
      .map(
        t => if (optionalConstraints.contains(f.constraint)) optional(t) else t
      )
  }

  def fieldDecl(f: FieldDecl): Doc =
    spread(List(ident(f.name) + char(':'), typeExpr(f)) ++ comment(f.comment))

  def enumMemberDecl(e: EnumDecl)(m: EnumMemberDecl): Doc = {
    val code = spread(List(text("override val code ="), strLit(m.value)))
    val description = spread(
      List(
        text("override val description ="),
        char('"') + text(m.comment.getOrElse(m.value)) + char('"')
      )
    )
    val prefix = spread(
      List(
        text("object"),
        ident(m.name),
        text("extends"),
        typeName(e.`type`),
        char('{')
      )
    )
    bracketBy(stack(List(code, description)))(prefix, char('}'))
  }

  def enumValuesVal(e: EnumDecl): Doc = {
    val prefix = text("lazy val values = Set[") + typeName(e.`type`) + text(
      "]("
    )
    val suffix = char(')')

    intercalate(
      (comma + line).grouped,
      e.members.toList.map(m => ident(m.name))
    ).tightBracketBy(prefix, suffix, tabSize)
  }

  def enumFromStringFunc(e: EnumDecl): Doc =
    bracketBy(text("s => values.find(_.code === s)"))(
      text("val fromString: String => Option[") +
        typeName(e.`type`) + text("] = {"),
      char('}')
    )

  def enumObjDecl(e: EnumDecl): Doc = {
    val prefix = spread(List(text("object"), typeName(e.`type`), text("{")))
    val suffix = char('}')

    intercalate(
      line,
      comment(e.comment) ++
        bracketBy(
          intercalate(
            line,
            e.members.map(enumMemberDecl(e) _).toList ++
              List(line + enumValuesVal(e), line + enumFromStringFunc(e))
          )
        )(prefix, suffix).pure[List]
    )
  }

  def enumTypeDecl(e: EnumDecl): Doc = {
    val prefix = spread(
      List(text("sealed"), text("trait"), typeName(e.`type`), char('{'))
    )
    val suffix = char('}')

    val t = bracketBy(
      stack(List(text("val code: String"), text("val description: String")))
    )(prefix, suffix)

    stack(comment(e.comment) ++ t.pure[List])
  }

  def enumDecl(e: EnumDecl): Doc =
    stack(List(enumTypeDecl(e), enumObjDecl(e)))

  def newtypeDecl(t: NewtypeDecl): Doc = {
    val t1 = spread(
      List(text("type"), typeName(t.`type`), eqSign, typeName(t.baseType))
    )
    stack(comment(t.comment) ++ t1.pure[List])
  }

  def typeDecl(t: TypeDecl): Doc = t match {
    case v: ComplexTypeDecl => complexTypeDecl(v)
    case v: EnumDecl => enumDecl(v)
    case v: NewtypeDecl => newtypeDecl(v)
  }

  def typesDecl(t: TypesDecl): Doc =
    intercalate(line + line, t.value.map(typeDecl _).toList)

  def importDecl(i: ImportDecl): Doc =
    spread(List(text("import"), text(i.path)))

  def importsDecl(i: ImportsDecl): Doc =
    stack(i.value.map(importDecl _).toList)

  def moduleDecl(m: ModuleDecl): Doc =
    intercalate(
      line + line,
      config.header.map(text _).toList ++
        m.imports.map(importsDecl _).toList ++
        m.types.map(typesDecl _).toList
    )
}
