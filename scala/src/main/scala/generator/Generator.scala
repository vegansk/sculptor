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

  def mask(s: String, maskReservedWords: Boolean): String =
    maskReservedWords match {
      case true if reservedWords.contains(s) =>
        s"`$s`"
      case _ => s
    }

  def ident(i: Ident, maskReservedWords: Boolean = true): Doc =
    text(mask(i.value, maskReservedWords))

  def bracketBy(d: Doc)(left: Doc, right: Doc): Doc =
    left + (lineBreak + d).nested(tabSize) + lineBreak + right

  def qNameString(name: QName, maskReservedWords: Boolean = true): String =
    name.path.map(i => mask(i.value, maskReservedWords)).toList.mkString(".")

  def qName(name: QName, maskReservedWords: Boolean = true): Doc =
    text(qNameString(name, maskReservedWords))

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

  def typeNameString(t: TypeRef, maskReservedWords: Boolean = true): String =
    t match {
      case TypeRef.std(v) => mask(v.value, maskReservedWords)
      case TypeRef.defined(v) => mask(v.value, maskReservedWords)
      case TypeRef.external(v) => qNameString(v, maskReservedWords)
    }

  def typeName(t: TypeRef, maskReservedWords: Boolean = true): Doc =
    text(typeNameString(t, maskReservedWords))

  def mkInstanceVal(t: TypeRef, typeClass: String): Doc =
    spread(
      List(
        text("implicit val"),
        typeName(t, false) + text(typeClass) + char(':'),
        text(typeClass) + char('[') + typeName(t) + char(']'),
        eqSign
      )
    ) + space

  def mkInstanceByCall(t: TypeRef, typeClass: String, call: String): Doc =
    text(typeClass) + char('[') + typeName(t) + text("].") + text(call)

  def catsEqInstance(t: TypeRef): Option[Doc] =
    config.parameters.generateCatsEq match {
      case true =>
        Option(mkInstanceVal(t, "Eq") + text("Eq.fromUniversalEquals"))
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

  def complexTypeCirceEncoder(ct: ComplexTypeDecl): Doc = {
    val prefix = mkInstanceVal(ct.`type`, "Encoder") + text(
      s"Encoder.instance[${typeNameString(ct.`type`)}] { v =>"
    )
    val postfix = char('}')

    val fields = ct.fields.toList.map { f =>
      spread(List(strLit(f.name.value), text(":= v.") + ident(f.name)))
    }

    val obj =
      bracketBy(intercalate(comma + line, fields))(text("Json.obj("), char(')'))

    bracketBy(obj)(prefix, postfix)
  }

  def complexTypeCirceDecoder(ct: ComplexTypeDecl): Doc = {
    val prefix = mkInstanceVal(ct.`type`, "Decoder") + text(
      s"Decoder.instance[${typeNameString(ct.`type`)}] { c =>"
    )
    val postfix = char('}')

    val fieldsDecls = ct.fields.toList.map { f =>
      spread(
        List(
          ident(f.name),
          text("<-"),
          text("c.downField(") + strLit(f.name.value) + text(").as[") + typeExpr(
            f
          ) + char(']')
        )
      )
    }

    val fieldsList =
      intercalate(comma + space, ct.fields.toList.map(f => ident(f.name)))

    val `for` = bracketBy(stack(fieldsDecls))(text("for {"), char('}')) +
      space + text("yield") + space +
      typeName(ct.`type`) + char('(') + fieldsList + char(')')

    bracketBy(`for`)(prefix, postfix)
  }

  def complexTypeCirceCodecs(ct: ComplexTypeDecl): List[Doc] =
    config.parameters.generateCirceCodecs match {
      case true =>
        List(complexTypeCirceEncoder(ct), complexTypeCirceDecoder(ct))
      case false => Nil
    }

  def complexTypeObjectDecl(ct: ComplexTypeDecl): Option[Doc] = {
    val elements = NEL
      .fromList(
        catsEqInstance(ct.`type`).toList ++
          complexTypeCirceCodecs(ct)
      )
      .map(_.toList)

    elements.map { el =>
      val prefix = text("object ") + typeName(ct.`type`) + text(" {")
      val postfix = char('}')

      bracketBy(intercalate(line * 2, el))(prefix, postfix)
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

  def enumCirceCodecs(e: EnumDecl): List[Doc] =
    config.parameters.generateCirceCodecs match {
      case true =>
        List(
          mkInstanceVal(e.`type`, "Encoder") + mkInstanceByCall(
            TypeRef.definedFrom("String"),
            "Encoder",
            "contramap(_.code)"
          ),
          mkInstanceVal(e.`type`, "Decoder") + mkInstanceByCall(
            TypeRef.definedFrom("String"),
            "Decoder",
            "emap(fromString(_).toRight(\"Invalid enum value\"))"
          )
        )
      case _ => Nil
    }

  def enumObjectDecl(e: EnumDecl): Doc = {
    val prefix = spread(List(text("object"), typeName(e.`type`), text("{")))
    val suffix = char('}')

    bracketBy(
      intercalate(line, e.members.map(enumMemberDecl(e) _).toList) + line * 2 + intercalate(
        line * 2,
        List(enumValuesVal(e), enumFromStringFunc(e)) ++ catsEqInstance(
          e.`type`
        ).toList ++ enumCirceCodecs(e)
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

  def newtypeCirceCodecs(t: NewtypeDecl): List[Doc] =
    config.parameters.generateCirceCodecs match {
      case true =>
        List(
          mkInstanceVal(t.`type`, "Encoder") + mkInstanceByCall(
            t.baseType,
            "Encoder",
            "contramap(_.value)"
          ),
          mkInstanceVal(t.`type`, "Decoder") + mkInstanceByCall(
            t.baseType,
            "Decoder",
            s"map(${typeNameString(t.`type`)}(_))"
          )
        )
      case _ => Nil
    }

  def newtypeClassDecl(t: NewtypeDecl): Doc = {
    val prefix = text("final case class ") + typeName(t.`type`) + char('(')
    val postfix = char(')')

    bracketBy(text("value: ") + typeName(t.baseType))(prefix, postfix)
  }

  def newtypeObjectDecl(t: NewtypeDecl): Option[Doc] = {
    val elements = NEL
      .fromList(
        catsEqInstance(t.`type`).toList ++
          newtypeCirceCodecs(t)
      )
      .map(_.toList)

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
