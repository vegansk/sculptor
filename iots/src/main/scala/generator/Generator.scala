package sculptor
package iots
package generator

import cats._
import cats.data.{NonEmptyList => NEL}
import cats.implicits._
import org.typelevel.paiges._

class Generator(config: Config) {

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

  def qName(name: QName): Doc =
    intercalate(dot, name.path.map(ident).toList)

  def array(`type`: Doc): Doc =
    qName(QName(NEL.of(config.iotsNs, Ident("array")))) + char('(') + `type` + char(
      ')'
    )

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

  def complexTypeIntfDecl(ct: ComplexTypeDecl): Doc =
    exportPrefix(ct.exported) +
      spread(
        List(
          text("interface"),
          ident(ct.name),
          text("extends"),
          typeOfExpr(ct.constName),
          text("{}")
        )
      )

  def typeConst(t: TypeRef): Doc =
    qName(t.constName)

  def typeName(t: TypeRef): Doc = t.`type` match {
    case TypeName.std(v) => ident(v)
    case TypeName.custom(v) => qName(v)
  }

  def typeExpr(f: FieldDecl): Doc = {
    val nullableConstraints =
      List(FieldConstraint.Nullable, FieldConstraint.OptionalNullable)
    (typeConst(f.`type`): Id[Doc])
      .map(t => if (f.array) array(t) else t)
      .map(
        t =>
          if (nullableConstraints.contains(f.constraint)) nullable(t) else (t)
      )
  }

  def fieldDecl(f: FieldDecl): Doc =
    ident(f.name) + char(':') + space + typeExpr(f)

  private val intersection: Doc = qName(
    QName.of(config.iotsNs, Ident("intersection"))
  )
  private val interface: Doc = qName(
    QName.of(config.iotsNs, Ident("interface"))
  )
  private val partial: Doc = qName(QName.of(config.iotsNs, Ident("partial")))

  def complexTypeConstDecl(ct: ComplexTypeDecl): Doc = {
    val prefix = exportPrefix(ct.exported) +
      spread(List(const, ident(ct.constName), eqSign, intersection)) +
      text("([")
    val suffix = text("],") + space + char('"') + text(ct.name.value) + text(
      "\")"
    )

    val requiredConstraints =
      List(FieldConstraint.Required, FieldConstraint.Nullable)

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

    intercalate(
      comma + line,
      List(
        fields(interface, requiredConstraints.contains),
        fields(partial, requiredConstraints.contains(_).unary_!)
      ).foldMap(_.toList)
    ).tightBracketBy(prefix, suffix)
  }

  def complexTypeDecl(ct: ComplexTypeDecl): Doc =
    stack(List(complexTypeConstDecl(ct), complexTypeIntfDecl(ct)))

  def enumMemberDecl(m: EnumMemberDecl): Doc =
    spread(List(ident(m.name), eqSign, strLit(m.value)))

  def enumTypeDecl(e: EnumDecl): Doc = {
    val prefix = exportPrefix(e.exported) +
      spread(List(text("enum"), ident(e.name), text("{")))
    val suffix = char('}')

    intercalate(comma + line, e.members.map(enumMemberDecl _).toList)
      .tightBracketBy(prefix, suffix)
  }

  private val mkStringEnum: Doc = qName(
    QName.of(config.iotsExtraNs, Ident("mkStringEnum"))
  )

  def enumConstDecl(e: EnumDecl): Doc =
    exportPrefix(e.exported) +
      spread(
        List(
          const,
          ident(e.constName),
          eqSign,
          mkStringEnum + char('<') + ident(e.name) +
            text(">(") + ident(e.name) + comma +
            space + strLit(e.name.value) + char(')')
        )
      )

  def enumDecl(e: EnumDecl): Doc =
    stack(List(enumTypeDecl(e), enumConstDecl(e)))

  def newtypeConstDecl(t: NewtypeDecl): Doc =
    exportPrefix(t.exported) +
      spread(List(const, ident(t.constName), eqSign, typeConst(t.baseType)))

  def newtypeTypeDecl(t: NewtypeDecl): Doc =
    exportPrefix(t.exported) +
      spread(List(`type`, ident(t.name), eqSign, typeName(t.baseType)))

  def newtypeDecl(t: NewtypeDecl): Doc =
    stack(List(newtypeConstDecl(t), newtypeTypeDecl(t)))

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
      m.imports.map(importsDecl _).toList ++
        m.types.map(typesDecl _).toList
    )
}
