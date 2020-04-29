package sculptor.scalagen
package impl
package features

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._

object TapirSchema extends Feature with GenHelpers {

  def mkPrefix(r: TypeRef): Doc = {
    val typ = createTypeRef(r)
    createTypeclassDef(r, "Schema", true) +
      Doc.text("implicitly[Derived[Schema[") + typ + Doc.text("]]].value")
  }

  def mkDescription(comment: Option[String]): Option[Doc] =
    comment.map(c => Doc.text(s""".description("$c")"""))

  def mkSchema(t: TypeDef, fields: List[FieldDef]): Result[List[Doc]] =
    getIndent.map { indent =>
      val result = mkPrefix(t.ref) + Doc.lineNoFlat.nested(indent) +
        Doc
          .intercalate(
            Doc.lineNoFlat,
            mkDescription(t.comment).toList ++ (
              fields.foldMap(f => f.comment.map((f.name.name, _)).toList).map {
                case (name, comment) =>
                  Doc.text(s""".modify(_.$name)(_.description("$comment"))""")
              }
            )
          )
          .nested(indent)

      result.pure[List]
    }

  override def handleNewtype(n: Newtype): Result[List[Doc]] =
    mkSchema(n, Nil)

  override def handleRecord(r: Record): Result[List[Doc]] =
    mkSchema(r, r.fields.toList)

}
