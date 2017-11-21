package sculptor
package iots

import cats.data.{NonEmptyList => NEL}
import cats.implicits._
import org.typelevel.paiges._

object generator {

  import ast._

  final case class Config(iotsNs: Ident, iotsExtraNs: Ident)

  def apply(config: Config): impl = new impl(config)

  class impl(config: Config) {

    import ast._

    import Doc._

    val dot: Doc = char('.')

    val eqSign: Doc = char('=')

    val asterisk: Doc = char('*')

    val const: Doc = text("const")

    val `import`: Doc = text("import")

    def strLit(v: String): Doc = char('"') + text(v) + char('"')

    def ident(i: Ident): Doc = text(i.value)

    def qName(name: QName): Doc =
      intercalate(dot, name.path.map(ident).toList)

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

    def typeName(t: TypeName): Doc = t match {
      case TypeName.std(n) =>
        qName(QName(NEL.of(config.iotsNs, n)))
      case TypeName.custom(n) => qName(n)
    }

    def fieldDecl(f: FieldDecl): Doc =
      ident(f.name) + char(':') + space + typeName(f.`type`)

    private val intersection: Doc = typeName(
      TypeName.std(Ident("intersection"))
    )
    private val interface: Doc = typeName(TypeName.std(Ident("interface")))
    private val partial: Doc = typeName(TypeName.std(Ident("partial")))

    def complexTypeConstDecl(ct: ComplexTypeDecl): Doc = {
      val prefix = exportPrefix(ct.exported) +
        spread(List(const, ident(ct.constName), eqSign, intersection)) +
        text("([")
      val suffix = text("],") + space + char('"') + text(ct.name.value) + text(
        "\")"
      )

      val requiredFields = {
        val prefix = interface + text("({")
        val suffix = text("})")
        val fields = ct.fields.filter(_.optional === false).map(fieldDecl _)
        intercalate(comma + line, fields)
          .tightBracketBy(prefix, suffix)
      }

      val optionalFields = {
        val prefix = partial + text("({")
        val suffix = text("})")
        val fields = ct.fields.filter(_.optional === true).map(fieldDecl _)
        intercalate(comma + line, fields)
          .tightBracketBy(prefix, suffix)
      }

      intercalate(comma + line, List(requiredFields, optionalFields))
        .tightBracketBy(prefix, suffix)
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

    private val mkStringEnum: Doc = typeName(
      TypeName.custom(QName.of(config.iotsExtraNs, Ident("mkStringEnum")))
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

    def typeDecl(t: TypeDecl): Doc = t match {
      case v: ComplexTypeDecl => complexTypeDecl(v)
      case v: EnumDecl => enumDecl(v)
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

}
