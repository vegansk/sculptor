package sculptor
package iots
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

  def bracketBy(d: Doc)(left: Doc, right: Doc): Doc =
    left + (lineBreak + d).nested(tabSize) + lineBreak + right

  def strLit(v: String): Doc = char('"') + text(v) + char('"')

  def ident(i: Ident): Doc = text(i.value)

  def decapitalize(s: String): String = {
    val (x, xs) = s.splitAt(1)
    x.toLowerCase + xs
  }

  def qNameString(name: QName): String =
    name.path.map(_.value).toList.mkString(".")

  def qName(name: QName): Doc =
    text(qNameString(name))

  def array(`type`: Doc): Doc =
    qName(QName(NEL.of(config.iotsNs, Ident("array")))) + char('(') + `type` + char(
      ')'
    )

  def comment(c: Option[Comment]): List[Doc] =
    c.filter(_ => config.generateComments)
      .map(_.replace("\n", " "))
      .fold[List[Doc]](Nil)(t => List(text("/** ") + text(t) + text(" */")))

  def nativeArray(`type`: Doc): Doc =
    `type` + text("[]")

  def nullable(`type`: Doc): Doc =
    qName(QName(NEL.of(config.iotsNs, Ident("union")))) +
      text("([") +
      intercalate(
        comma,
        List(`type`, qName(QName.of(config.iotsNs, Ident("null"))))
      ) +
      text("])")

  private def typeOfExpr(name: Ident): Doc =
    qName(QName(NEL.of(config.iotsNs, Ident("TypeOf")))) +
      text("<typeof") + space + ident(name) + char('>')

  def exportPrefix(exported: Boolean): Doc =
    if (exported)
      text("export") + space
    else
      empty

  private val nullableConstraints =
    List(FieldConstraint.Nullable, FieldConstraint.OptionalNullable)

  private val requiredConstraints =
    List(FieldConstraint.Required, FieldConstraint.Nullable)

  def nativeTypeExpr(f: FieldDecl): Doc = {
    (typeName(f.`type`): Id[Doc])
      .map(t => if (f.array) nativeArray(t) else t)
  }

  def nativeFieldDecl(f: FieldDecl): Doc = {
    val optional =
      if (!requiredConstraints.contains(f.constraint)) char('?')
      else empty
    spread(
      List(ident(f.name) + optional + char(':'), nativeTypeExpr(f)) ++ comment(
        f.comment
      )
    )
  }

  def complexTypeNativeIntfDecl(ct: ComplexTypeDecl): Doc = {
    val base = ct.baseType.fold(List[Doc]()) { bt =>
      List(text("extends"), typeName(bt))
    }
    val prefix = exportPrefix(ct.exported) +
      spread(
        List(text("interface"), ident(ct.`type`.name)) ++ base ++ List(
          text("{")
        )
      )
    val postfix = char('}')
    stack(
      comment(ct.comment) ++
        intercalate(comma + line, ct.fields.toList.map(nativeFieldDecl _))
          .tightBracketBy(prefix, postfix)
          .pure[List]
    )
  }

  def complexTypeIotsIntfDecl(ct: ComplexTypeDecl): Doc = {
    val i = exportPrefix(ct.exported) +
      spread(
        List(
          text("interface"),
          ident(ct.`type`.name),
          text("extends"),
          typeOfExpr(ct.`type`.constName),
          text("{}")
        )
      )
    stack(comment(ct.comment) ++ i.pure[List])
  }

  def complexTypeIntfDecl(ct: ComplexTypeDecl): Doc =
    config.nativeTypes match {
      case true => complexTypeNativeIntfDecl(ct)
      case _ => complexTypeIotsIntfDecl(ct)
    }

  def typeConst(t: TypeRef): Doc = t match {
    case TypeRef.std(v) => qName(QName.of(config.iotsNs, v))
    case TypeRef.defined(_, v) => ident(v)
    case TypeRef.external(_, v) => qName(v)
  }

  def typeNameString(t: TypeRef, maskReservedWords: Boolean = true): String =
    t match {
      case TypeRef.std(v) => v.value
      case TypeRef.defined(v, _) => v.value
      case TypeRef.external(v, _) => qNameString(v)
    }

  def typeName(t: TypeRef): Doc =
    text(typeNameString(t))

  def typeExpr(f: FieldDecl): Doc = {
    (typeConst(f.`type`): Id[Doc])
      .map(t => if (f.array) array(t) else t)
      .map(
        t =>
          if (nullableConstraints.contains(f.constraint)) nullable(t) else (t)
      )
  }

  def fieldDecl(f: FieldDecl): Doc =
    spread(List(ident(f.name) + char(':'), typeExpr(f)) ++ comment(f.comment))

  private val intersection: Doc = qName(
    QName.of(config.iotsNs, Ident("intersection"))
  )
  private val interface: Doc = qName(
    QName.of(config.iotsNs, Ident("interface"))
  )
  private val partial: Doc = qName(QName.of(config.iotsNs, Ident("partial")))

  def complexTypeConstDecl(ct: ComplexTypeDecl): Doc = {
    val prefix = exportPrefix(ct.exported) +
      spread(List(const, ident(ct.`type`.constName), eqSign, intersection)) +
      text("([")
    val suffix = text("],") + space + char('"') + text(ct.`type`.name.value) + text(
      "\")"
    )

    def fields(prefix: Doc,
               constraint: FieldConstraint => Boolean): Option[Doc] =
      ct.fields.filter(f => constraint(f.constraint)).toNel.fold(none[Doc]) {
        fields =>
          val p = prefix + text("({")
          val s = text("})")
          intercalate(comma + line, fields.toList.map(fieldDecl _))
            .tightBracketBy(p, s)
            .some
      }

    stack(
      comment(ct.comment) ++
        intercalate(
          comma + line,
          List(
            ct.baseType.map(typeConst _),
            fields(interface, requiredConstraints.contains),
            fields(partial, requiredConstraints.contains(_).unary_!)
          ).foldMap(_.toList)
        ).tightBracketBy(prefix, suffix).pure[List]
    )
  }

  def complexTypeDecl(ct: ComplexTypeDecl): Doc =
    stack(List(complexTypeConstDecl(ct), complexTypeIntfDecl(ct)))

  def enumMemberDecl(m: EnumMemberDecl): Doc =
    spread(List(ident(m.name), eqSign, strLit(m.value)) ++ comment(m.comment))

  def enumTypeDecl(e: EnumDecl): Doc = {
    val prefix = exportPrefix(e.exported) +
      spread(List(text("enum"), ident(e.`type`.name), text("{")))
    val suffix = char('}')

    stack(
      comment(e.comment) ++
        intercalate(comma + line, e.members.map(enumMemberDecl _).toList)
          .tightBracketBy(prefix, suffix)
          .pure[List]
    )
  }

  def enumConstDecl(e: EnumDecl): Doc = {
    val c = exportPrefix(e.exported) +
      spread(
        List(
          const,
          ident(e.`type`.constName),
          eqSign,
          text("mkStringEnum") + char('<') + ident(e.`type`.name) +
            text(">(") + ident(e.`type`.name) + comma +
            space + strLit(e.`type`.name.value) + char(')')
        )
      )

    stack(comment(e.comment) ++ c.pure[List])
  }

  def enumDocumentationGetter(e: EnumDecl): Option[Doc] =
    config.generateEnumsDocumentationGetters match {
      case true => {
        val prefix = exportPrefix(e.exported) +
          text("const " + decapitalize(typeNameString(e.`type`)) + "Desc: [") +
          typeName(e.`type`) + text(", string][] = [")
        val postfix = char(']')
        val elements = e.members.toList.map { m =>
          char('[') + typeName(e.`type`) + dot + ident(m.name) +
            text(", ") + strLit(m.comment.getOrElse(m.value)) + char(']')
        }

        bracketBy(intercalate(comma + line, elements))(prefix, postfix).some
      }
      case false => None
    }

  def enumDecl(e: EnumDecl): Doc =
    stack(
      List(enumTypeDecl(e), enumConstDecl(e)) ++ enumDocumentationGetter(e).toList
    )

  def newtypeConstDecl(t: NewtypeDecl): Doc = {
    val c = exportPrefix(t.exported) +
      spread(
        List(const, ident(t.`type`.constName), eqSign, typeConst(t.baseType))
      )
    stack(comment(t.comment) ++ c.pure[List])
  }

  def newtypeTypeDecl(t: NewtypeDecl): Doc = {
    val t1 = exportPrefix(t.exported) +
      spread(List(`type`, ident(t.`type`.name), eqSign, typeName(t.baseType)))
    stack(comment(t.comment) ++ t1.pure[List])
  }

  def newtypeDecl(t: NewtypeDecl): Doc =
    stack(List(newtypeConstDecl(t), newtypeTypeDecl(t)))

  lazy val getStringEnumValuesImpl: Doc = text(
    """function getStringEnumValues(o: object): string[] {
  return Object.keys(o)
    .map(_ => (o as { [n: string]: any })[_])
    .filter(v => typeof v === "string")
}"""
  )

  lazy val mkStringEnumImpl: Doc = {
    val t = config.iotsNs.value
    text(s"""function mkStringEnum<E>(e: object, name: string): $t.Type<E> {
  const values = getStringEnumValues(e)
  const newType: $t.Type<E> = {
    _A: $t._A,
    name,
    validate: (v, c) => values.indexOf(v) >= 0 ? $t.success<E>(v) : $t.failure<E>(v, c)
  }
  return newType
}""")
  }

  lazy val inlineMkStringEnum: Doc =
    stack(List(getStringEnumValuesImpl, mkStringEnumImpl))

  def typeDecl(t: TypeDecl): Doc = t match {
    case v: ComplexTypeDecl => complexTypeDecl(v)
    case v: EnumDecl => enumDecl(v)
    case v: NewtypeDecl => newtypeDecl(v)
  }

  def typesDecl(t: TypesDecl): Doc =
    intercalate(line + line, t.value.map(typeDecl _).toList)

  def importDecl(i: ImportDecl): Doc =
    spread(
      List(
        text("import"),
        asterisk,
        text("as"),
        ident(i.name),
        text("from"),
        strLit(i.path)
      )
    )

  def importsDecl(i: ImportsDecl): Doc =
    stack(i.value.map(importDecl _).toList)

  def moduleDecl(m: ModuleDecl): Doc =
    intercalate(
      line + line,
      config.header.map(text _).toList ++
        m.imports.map(importsDecl _).toList ++
        List(inlineMkStringEnum) ++
        m.types.map(typesDecl _).toList
    )
}
