package sculptor.scalagen
package impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._
import sculptor.common._

trait GenHelpers extends CommonGenHelpers {

  import ScalaIdent._

  def createTypeRef(r: TypeRef): Doc = {
    def generic(g: TypeRef.Generic) =
      createTypeExpr(g.name, Nil)

    def specialized(s: TypeRef.Specialized) =
      createTypeExpr00(s.name.asScalaId, s.parameters.map(createTypeRef))

    TypeRef.cata(specialized, generic)(r)
  }

  def createGenericParam(g: GenericDef): Doc =
    Doc.text(g.`type`.name.asScalaId) + g.`extends`.toNel
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

  def createTypeExpr00(name: String, parameters: List[Doc]): Doc =
    Doc.text(name) + createParameters0(parameters)

  def createTypeExpr(name: Ident, parameters: List[GenericDef]): Doc =
    createTypeExpr0(name.asScalaId, parameters)

  def createTypeExpr0(name: String, parameters: List[GenericDef]): Doc =
    Doc.text(name) + createParameters(parameters)

  def createField0(name: Ident, `type`: TypeRef): Doc =
    Doc.text(name.asScalaId) + Doc.text(": ") + createTypeRef(`type`)

  def createField(withComment: Boolean)(f: FieldDef): Doc =
    Doc.intercalate(
      line,
      List(
        optionalComment(withComment)(f.comment),
        createField0(f.name, f.`type`).some
      ).flattenOption
    )

  def sealedTrait(typ: Doc): Doc =
    Doc.text("sealed trait ") + typ

  def createTypeclassDef(r: TypeRef,
                         className: String,
                         classInParams: Boolean,
                         lazyInstance: Boolean): Doc = {
    val typ = createTypeRef(r)
    val (definition, name, params) = TypeRef.cata(
      s =>
        (
          if (s.parameters.isEmpty) {
            if (lazyInstance) "lazy val" else "val"
          } else {
            "def"
          },
          s.name.name.name,
          createParameters0(
            s.parameters.map(createTypeRef),
            if (classInParams) Doc.text(s":$className") else Doc.empty
          )
      ),
      g => ("val", g.name.name, Doc.empty)
    )(r)

    Doc.text(s"implicit $definition $name$className") + params +
      Doc.text(s": $className[") + typ + Doc.text("] =")
  }

  def caseClassPrefix(`type`: Doc): Doc =
    Doc.text("final case class ") + `type` + Doc.char('(')

  val newtypePostfix = Doc.text(") extends AnyVal")

  val caseClassPostfix = Doc.char(')')

  def caseObject(`type`: Ident): Doc =
    caseObject0(Doc.text(`type`.asScalaId))

  def caseObject0(`type`: Doc): Doc =
    Doc.text(s"case object ") + `type`

  def objectPrefix(`type`: Ident): Doc =
    objectPrefix0(Doc.text(`type`.asScalaId))

  def objectPrefix0(`type`: Doc): Doc =
    Doc.text("object ") + `type` + Doc.text(" {")

  val objectPostfix = Doc.char('}')

  val functionPostfix = Doc.char('}')

  val fieldDelim = Doc.char(',') + Doc.line

  val paramDelim = Doc.text(", ")

  val line = Doc.hardLine
  val dblLine = Doc.hardLine * 2

  def extend(what: Doc, `with`: Doc): Doc =
    what + Doc.text(" extends ") + `with`

  def adtSealedTrait(typ: Ident): Doc =
    adtSealedTrait0(Doc.text(typ.asScalaId))

  def adtSealedTrait0(typ: Doc): Doc =
    extend(sealedTrait((typ)), Doc.text("Product with Serializable"))

  def mkQuotedString(s: String): String = {
    val lines = s.linesIterator.toList
    if (lines.length > 1) {
      "\"\"\"" + lines.map("|" + _).mkString("\n") + "\"\"\".stripMargin"
    } else {
      "\"" + s + "\""
    }
  }
}
