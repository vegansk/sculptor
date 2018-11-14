package sculptor.scalagen
package impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._
import sculptor.common._

trait GenHelpers extends CommonGenHelpers {

  def createTypeRef(r: TypeRef): Doc = {
    def generic(g: TypeRef.Generic) =
      createTypeExpr(g.name.name, Nil)

    def specialized(s: TypeRef.Specialized) =
      createTypeExpr0(s.name.mkString("."), s.parameters.map(createTypeRef))

    TypeRef.cata(specialized, generic)(r)
  }

  def createGenericParam(g: GenericDef): Doc =
    Doc.text(g.`type`.name.name) + g.`extends`.toNel
      .map(
        l =>
          Doc.text(" <: ") + Doc
            .intercalate(Doc.text(" with "), l.toList.map(createTypeRef))
      )
      .getOrElse(Doc.empty)

  def createParameters0(l0: List[Doc], paramPostfix: Doc = Doc.empty): Doc = {
    l0.toNel
      .map { l =>
        Doc.char('[') + Doc.intercalate(
          Doc.text(", "),
          l.toList.map(_ + paramPostfix)
        ) + Doc
          .char(']')
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

  def createField(withComment: Boolean)(f: FieldDef): Doc =
    createField0(f.name, f.`type`) + Option(withComment)
      .filter(identity)
      .flatMap(_ => f.comment)
      .map(c => Doc.text(s" /* $c */"))
      .getOrElse(Doc.empty)

  def sealedTrait(typ: Doc): Doc =
    Doc.text("sealed trait ") + typ

  def caseClassPrefix(`type`: Doc): Doc =
    Doc.text("final case class ") + `type` + Doc.char('(')

  val newtypePostfix = Doc.text(") extends AnyVal")

  val caseClassPostfix = Doc.char(')')

  def caseObject(`type`: Doc): Doc =
    Doc.text("case object ") + `type`

  def objectPrefix(`type`: Doc): Doc =
    Doc.text("object ") + `type` + Doc.text(" {")

  val objectPostfix = Doc.char('}')

  val fieldDelim = Doc.char(',') + Doc.line

  val line = Doc.lineNoFlat
  val dblLine = Doc.lineNoFlatNoIndent + Doc.lineNoFlat

  def extend(what: Doc, `with`: Doc): Doc =
    what + Doc.text(" extends ") + `with`

  def adtSealedTrait(typ: Doc): Doc =
    extend(sealedTrait((typ)), Doc.text("Product with Serializable"))
}
