package sculptor
package scala
package generator

import cats._
import cats.data.{NonEmptyList => NEL}
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

  private val reservedWords = Set(
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "forSome",
    "if",
    "implicit",
    "import",
    "lazy",
    "match",
    "new",
    "null",
    "object",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "super",
    "this",
    "throw",
    "trait",
    "try",
    "true",
    "type",
    "val",
    "var",
    "while",
    "with",
    "yield"
  )

  def ident(i: Ident): Doc =
    reservedWords.contains(i.value) match {
      case true => char('`') + text(i.value) + char('`')
      case _ => text(i.value)
    }

  def bracketBy(d: Doc)(left: Doc, right: Doc): Doc =
    left + (lineBreak + d).nested(tabSize) + lineBreak + right

  def qName(name: QName): Doc =
    intercalate(dot, name.path.map(ident).toList)

  def array(`type`: Doc): Doc =
    text("List[") + `type` + char(']')

  def comment(c: Option[Comment]): List[Doc] =
    c.filter(_ => config.parameters.generateComments)
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

  def typeName(t: TypeRef): Doc = t match {
    case TypeRef.std(v) => ident(v)
    case TypeRef.defined(v) => ident(v)
    case TypeRef.external(v) => qName(v)
  }

  def catsEqInstance(t: TypeRef): Option[Doc] =
    config.parameters.generateCatsEq match {
      case true =>
        Option(
          text("lazy val ") + typeName(t) + text("Eq: Eq[") + typeName(t) + text(
            "] = Eq.fromUniversalEquals"
          )
        )
      case _ => None
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

  def complexTypeClassDecl(ct: ComplexTypeDecl): Doc = {
    val prefix = spread(List(text("final case class"), ident(ct.`type`.name))) + char(
      '('
    )
    val postfix = char(')')

    bracketBy(intercalate(comma + line, ct.fields.toList.map(fieldDecl _)))(
      prefix,
      postfix
    )
  }

  def complexTypeObjectDecl(ct: ComplexTypeDecl): Option[Doc] = {
    val elements = NEL.fromList(catsEqInstance(ct.`type`).toList).map(_.toList)

    elements.map { el =>
      val prefix = text("object ") + typeName(ct.`type`) + text(" {")
      val postfix = char('}')

      bracketBy(stack(el))(prefix, postfix)
    }
  }

  def complexTypeDecl(ct: ComplexTypeDecl): Doc =
    stack(
      comment(ct.comment) ++
        complexTypeClassDecl(ct).pure[List] ++
        complexTypeObjectDecl(ct).toList
    )

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
    bracketBy(text("s => values.find(_.code == s)"))(
      text("val fromString: String => Option[") +
        typeName(e.`type`) + text("] = {"),
      char('}')
    )

  def enumObjectDecl(e: EnumDecl): Doc = {
    val prefix = spread(List(text("object"), typeName(e.`type`), text("{")))
    val suffix = char('}')

    bracketBy(
      intercalate(line, e.members.map(enumMemberDecl(e) _).toList) + line * 2 + intercalate(
        line * 2,
        List(enumValuesVal(e), enumFromStringFunc(e)) ++ catsEqInstance(
          e.`type`
        ).toList
      )
    )(prefix, suffix)
  }

  def enumTypeDecl(e: EnumDecl): Doc = {
    val prefix = spread(
      List(text("sealed"), text("trait"), typeName(e.`type`), char('{'))
    )
    val suffix = char('}')

    bracketBy(
      stack(List(text("val code: String"), text("val description: String")))
    )(prefix, suffix)
  }

  def enumDecl(e: EnumDecl): Doc =
    stack(
      comment(e.comment) ++
        List(enumTypeDecl(e), enumObjectDecl(e))
    )

  def newtypeClassDecl(t: NewtypeDecl): Doc = {
    val prefix = text("final case class ") + typeName(t.`type`) + char('(')
    val postfix = char(')')

    bracketBy(text("value: ") + typeName(t.baseType))(prefix, postfix)
  }

  def newtypeObjectDecl(t: NewtypeDecl): Option[Doc] = {
    val elements = NEL.fromList(catsEqInstance(t.`type`).toList).map(_.toList)

    elements.map { xs =>
      val prefix = text("object ") + typeName(t.`type`) + text(" {")
      val postfix = char('}')
      bracketBy(intercalate(line * 2, xs))(prefix, postfix)
    }
  }

  def newtypeDecl(t: NewtypeDecl): Doc =
    stack(
      comment(t.comment) ++
        newtypeClassDecl(t).pure[List] ++
        newtypeObjectDecl(t).toList
    )

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

  def packageDecl: Option[Doc] =
    config.packageName.map { p =>
      text("package ") + text(p)
    }

  def moduleDecl(m: ModuleDecl): Doc =
    intercalate(
      line + line,
      config.header.map(text _).toList ++
        packageDecl.toList ++
        m.imports.map(importsDecl _).toList ++
        m.types.map(typesDecl _).toList
    )
}