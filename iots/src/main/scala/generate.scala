package sculptor
package iots

import org.typelevel.paiges._

import cats.data._

import scala.xml._

import shapeless._

object generator {

  import ast._

  private def fromName(n: Name): Doc = Doc.text(n)

  private def fromIdent(i: Ident): Doc = i match {
    case Ident(n, Some(i)) => fromIdent(i) + Doc.text(".") + fromName(n)
    case Ident(n, _) => fromName(n)
  }

  private val const_ = Doc.text("const") + Doc.space
  private def export_(what: Doc): Doc = Doc.text("export") + Doc.space + what
  private val type_ = Doc.text("type") + Doc.space

  private val _eq_ = Doc.space + Doc.text("=") + Doc.space

  private def fromTypeAlias(ta: TypeAlias): Doc = {
    val TypeAlias(n, r, rt) = ta
    export_(const_) + fromName(ta.constName) + _eq_ + fromIdent(r) + Doc.line +
      export_(type_) + fromName(n) + _eq_ + rt
      .map(fromIdent(_))
      .getOrElse(fromName(ta.constName)) + Doc.line
  }

  private object typeToDoc extends Poly1 {

    implicit val atTypeAlias: Case.Aux[TypeAlias, Doc] = at(fromTypeAlias(_))

  }

  private def fromType(`type`: Type): Doc = `type`.fold(typeToDoc)

  private def fromModule(module: Module): Doc =
    Doc.text("""import * as t from "io-ts"""") +
      Doc.line * 2 +
      Doc.intercalate(Doc.line, module.types.map(fromType(_)))

  def fromNode(n: Node): ValidatedNel[String, Doc] =
    sculptor.xsd
      .schemaAst(n)
      .andThen(xsd.foldModule(_))
      .map(fromModule(_))

}
