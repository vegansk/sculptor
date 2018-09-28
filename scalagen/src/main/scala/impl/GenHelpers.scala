package sculptor.scalagen
package impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._

trait GenHelpers {

  def createTypeRef(r: TypeRef): Doc = {
    def generic(g: TypeRef.Generic) =
      createTypeExpr(g.name.name, Nil)

    def specialized(s: TypeRef.Specialized) =
      createTypeExpr(s.name.mkString("."), s.parameters.map(createTypeRef))

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

  def createParameters(l0: List[Doc]): Doc =
    l0.toNel
      .map { l =>
        Doc.char('[') + Doc.intercalate(Doc.text(", "), l.toList) + Doc
          .char(']')
      }
      .getOrElse(Doc.empty)

  def createTypeExpr(name: String, parameters: List[Doc]): Doc =
    Doc.text(name) + createParameters(parameters)

  def createField0(name: Ident, `type`: TypeRef): Doc =
    Doc.text(name.name) + Doc.text(": ") + createTypeRef(`type`)

  def createField(f: FieldDef): Doc =
    createField0(f.name, f.`type`)

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
  val dblLine = line + line

  def extend(what: Doc, `with`: Doc): Doc =
    what + Doc.text(" extends ") + `with`

  def adtSealedTrait(typ: Doc): Doc =
    extend(sealedTrait((typ)), Doc.text("Product with Serializable"))

}
