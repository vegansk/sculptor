package sculptor.scalagen
package impl
package features

import org.typelevel.paiges._
import org.typelevel.paiges.instances._
import cats.implicits._

import sculptor.ast._

final class TapirSchema(adtTag: String, schemaLazyInstances: Boolean)
    extends Feature
    with GenHelpers {

  import ScalaIdent._

  // Should match CirceCodecs!
  private lazy val tagName: String =
    if (adtTag.isEmpty) "__tag" else adtTag

  private def mkDescription(comment: Option[String]): Option[Doc] =
    comment.map(c => Doc.text(s""".description(${mkQuotedString(c)})"""))

  private def mkRecordSchema(t: TypeDef, fields: List[FieldDef]): Result[Doc] =
    getIndent.map { indent =>
      createTypeclassDef(
        t.ref,
        "Schema",
        classInParams = true,
        lazyInstance = schemaLazyInstances
      ).line(
          recordLikeSchemaValue(createTypeRef(t.ref), t.comment, fields, indent)
        )
        .nested(indent)
    }

  // A `Schema` value for a record-like type (newtype, ADT constructor, etc.).
  private def recordLikeSchemaValue(typ: Doc,
                                    comment: Option[String],
                                    fields: List[FieldDef],
                                    indent: Int): Doc = {
    val base: Doc = Doc.text("Schema.derived[") + typ + Doc.text("]")
    val fields0: List[Doc] =
      fields.foldMap(f => f.comment.map((f.name.asScalaId, _)).toList).map {
        case (name, comment) =>
          Doc.text(
            s""".modify(_.$name)(_.description(${mkQuotedString(comment)}))"""
          )
      }

    (base :: mkDescription(comment).toList ++: fields0)
      .intercalate(Doc.line)
      .nested(indent)
  }

  private def mkEnumSchema(t: Enum): Result[Doc] = {
    val description: Option[String] = {
      val title = t.comment
      val elements = t.values.toList
        .flatMap(v => v.comment.map(c => s"* `${v.value}` - $c").toList)
        .toNel
        .map(_.toList.mkString("\n"))
      val description = List(title, elements).flattenOption match {
        case Nil => None
        case xs => Some(xs.mkString("\n\n"))
      }
      description.map(mkQuotedString(_))
    }
    getIndent.map { indent =>
      val base = Doc.text("Schema(SchemaType.SString())")
      val value = (
        base ::
          List(
          description.map(
            d => Doc.text(".description(") + Doc.text(d) + Doc.text(")")
          ),
          Doc
            .text(
              s""".validate(Validator.enumeration(values, x => Some(asString(x)), Some(Schema.SName("${t.name.name}"))))"""
            )
            .some
        ).flattenOption
      ).intercalate(Doc.line)
        .nested(indent)

      createTypeclassDef(
        t.ref,
        "Schema",
        classInParams = true,
        lazyInstance = schemaLazyInstances
      ).line(value)
        .nested(indent)
    }
  }

  private def mkAdtSchema(t: ADT): Result[Doc] =
    getIndent.map { indent =>
      def schemaValName(c: ADTConstructor) =
        c.name.name + "Schema"

      def schemaValDefinition(c: ADTConstructor): Doc = {
        val typ = if (c.fields.isEmpty && c.parameters.isEmpty) {
          // In this case the constructor is represented by a `case object`.
          Doc.text(c.name.asScalaId + ".type")
        } else createTypeRef(c.ref)

        Doc
          .text(s"val ${schemaValName(c)} =")
          .line(recordLikeSchemaValue(typ, c.comment, c.fields, indent))
          .nested(indent)
      }

      val schemaVals = t.constructors.map(schemaValDefinition).toList
      val base = Doc.text("val base = Schema.derived[") + createTypeRef(t.ref) + Doc
        .text("]")

      val mappings = {
        Doc
          .text("val mappings = Map(")
          .line(
            t.constructors
              .map(
                c =>
                  Doc.spaces(indent) + Doc.text(s""""${c.name.name}" -> (${schemaValName(
                    c
                  )}.schemaType -> ${schemaValName(c)}.name)""")
              )
              .intercalate(Doc.text(",") + Doc.line)
          )
          .line(").collect {")
          .line(
            Doc.spaces(indent) +
              Doc.text(
                "case (k, (_: SchemaType.SProduct[_], Some(name))) => (k, SchemaType.SRef(name))"
              )
          )
          .line("}")
      }

      val fixDiscriminator = Doc.text(
        // Not indented properly because that would be a pain.
        // https://gitlab.com/vegansk/sculptor/-/issues/147
        s"""|base.copy(
            |  schemaType = base.schemaType match {
            |    case sc: SchemaType.SCoproduct[_] =>
            |      sc.addDiscriminatorField(
            |        FieldName("${tagName}", "${tagName}"),
            |        discriminatorMapping = mappings
            |      )
            |    case _ => sys.error("unexpected schemaType")
            |  }
            |)""".stripMargin
      )

      List[Doc](
        schemaVals.intercalate(Doc.line),
        base,
        mappings,
        fixDiscriminator
      ).intercalate(
          // I've tried to separate the blocks here, but that messes up the test.
          // https://gitlab.com/vegansk/sculptor/-/issues/146
          Doc.line
          //Doc.line + Doc.line
        )
        .bracketBy(
          createTypeclassDef(
            t.ref,
            "Schema",
            classInParams = true,
            lazyInstance = schemaLazyInstances
          ).space("{"),
          Doc.text("}")
        )
    }

  private def mkNewtypeSchema(n: Newtype): Result[Doc] =
    getIndent.map { indent =>
      val typeclassDef = createTypeclassDef(
        n.ref,
        "Schema",
        classInParams = true,
        lazyInstance = schemaLazyInstances
      )
      val baseSchema = Doc.text("implicitly[Schema[") + createTypeRef(
        n.baseType
      ) + Doc.text("]]")
      val map =
        Doc.text(s""".map(x => Some(${n.name.asScalaId}(x)))(_.value)""")
      val description = mkDescription(n.comment)
      val name = Doc.text(s""".name(Schema.SName("${n.name.name}"))""")
      val body =
        baseSchema
          .line(
            List(map.some, description, name.some).flattenOption
              .intercalate(Doc.line)
          )
          .nested(indent)
      typeclassDef
        .line(body)
        .nested(indent)
    }

  override def handleNewtype(n: Newtype): Result[List[Doc]] =
    mkNewtypeSchema(n).map(List(_))

  override def handleRecord(r: Record): Result[List[Doc]] =
    mkRecordSchema(r, r.fields.toList).map(List(_))

  override def handleEnum(e: Enum): Result[List[Doc]] =
    mkEnumSchema(e).map(List(_))

  override def handleADT(a: ADT): Result[List[Doc]] =
    mkAdtSchema(a).map(List(_))
}
