package sculptor.scalagen
package impl
package features

import org.typelevel.paiges._
import org.typelevel.paiges.instances._
import cats.implicits._

import sculptor.ast._

final class TapirSchema(adtTag: String) extends Feature with GenHelpers {

  // Should match CirceCodecs!
  private lazy val tagName: String =
    if (adtTag.isEmpty) "__tag" else adtTag

  private def mkDescription(comment: Option[String]): Option[Doc] =
    comment.map(c => Doc.text(s""".description("$c")"""))

  private def mkRecordSchema(t: TypeDef, fields: List[FieldDef]): Result[Doc] =
    getIndent.map { indent =>
      createTypeclassDef(t.ref, "Schema", true)
        .line(
          recordLikeSchemaValue(createTypeRef(t.ref), t.comment, fields, indent)
        )
        .nested(indent)
    }

  // A `Schema` value for a record-like type (newtype, ADT constructor, etc.).
  private def recordLikeSchemaValue(typ: Doc,
                                    comment: Option[String],
                                    fields: List[FieldDef],
                                    indent: Int): Doc = {
    val base: Doc = Doc.text("Schema.derive[") + typ + Doc.text("]")
    val fields0: List[Doc] =
      fields.foldMap(f => f.comment.map((f.name.name, _)).toList).map {
        case (name, comment) =>
          Doc.text(s""".modify(_.$name)(_.description("$comment"))""")
      }

    (base :: mkDescription(comment).toList ++: fields0)
      .intercalate(Doc.line)
      .nested(indent)
  }

  private def mkDerivedValidator(t: TypeDef): Result[Doc] =
    getIndent.map { indent =>
      val typ = createTypeRef(t.ref)
      createTypeclassDef(t.ref, "Validator", true)
        .line(Doc.text("Validator.derive[") + typ + Doc.text("]"))
        .nested(indent)
    }

  private def mkEnumSchema(t: TypeDef): Result[Doc] =
    getIndent.map { indent =>
      val base = Doc.text("Schema(SchemaType.SString)")
      val value = (base :: mkDescription(t.comment).toList)
        .intercalate(Doc.line)
        .nested(indent)

      createTypeclassDef(t.ref, "Schema", true)
        .line(value)
        .nested(indent)
    }

  private def mkEnumValidator(t: Enum): Result[Doc] =
    getIndent.map { indent =>
      val typ = createTypeRef(t.ref)
      createTypeclassDef(t.ref, "Validator", true)
        .line(Doc.text("Validator.enum[") + typ + Doc.text("]"))
        .nested(indent)
    }

  private def mkAdtSchema(t: ADT): Result[Doc] =
    getIndent.map { indent =>
      def schemaValName(c: ADTConstructor) =
        c.name.name + "Schema"

      def schemaValDefinition(c: ADTConstructor): Doc = {
        val typ = if (c.fields.isEmpty && c.parameters.isEmpty) {
          // In this case the constructor is represented by a `case object`.
          Doc.text(c.ref.asString + ".type")
        } else createTypeRef(c.ref)

        (Doc.text(s"implicit val ${schemaValName(c)}: Schema[") +
          typ +
          Doc.text("] ="))
          .line(recordLikeSchemaValue(typ, c.comment, c.fields, indent))
          .nested(indent)
      }

      val schemaVals = t.constructors.map(schemaValDefinition).toList
      val ignores = t.constructors.map { c =>
        Doc.text(s"mouse.ignore(${schemaValName(c)})")
      }.toList
      val base = Doc.text("val base = Schema.derive[") + createTypeRef(t.ref) + Doc
        .text("]")

      val mappings = {
        Doc
          .text("val mappings = Map(")
          .line(
            t.constructors
              .map(
                c =>
                  Doc.spaces(indent) + Doc.text(
                    s""""${c.name.name}" -> ${schemaValName(c)}.schemaType"""
                )
              )
              .intercalate(Doc.text(",") + Doc.line)
          )
          .line(").collect {")
          .line(
            Doc.spaces(indent) +
              Doc.text(
                "case (k, sp: SchemaType.SProduct) => (k, SchemaType.SRef(sp.info))"
              )
          )
          .line("}")
      }

      val fixDiscriminator = Doc.text(
        // Not indented properly because that would be a pain.
        // https://gitlab.com/vegansk/sculptor/-/issues/147
        s"""|base.copy(
            |  schemaType = base.schemaType match {
            |    case sc: SchemaType.SCoproduct =>
            |      sc.addDiscriminatorField(
            |        FieldName("${tagName}", "${tagName}"),
            |        discriminatorMappingOverride = mappings
            |      )
            |    case _ => sys.error("unexpected schemaType")
            |  }
            |)""".stripMargin
      )

      List[Doc](
        schemaVals.intercalate(Doc.line),
        ignores.intercalate(Doc.line),
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
          createTypeclassDef(t.ref, "Schema", true).space("{"),
          Doc.text("}")
        )
    }

  override def handleNewtype(n: Newtype): Result[List[Doc]] =
    List(
      // It just so happens that this works for newtypes as well.
      mkRecordSchema(n, Nil),
      mkDerivedValidator(n)
    ).sequence

  override def handleRecord(r: Record): Result[List[Doc]] =
    List(mkRecordSchema(r, r.fields.toList), mkDerivedValidator(r)).sequence

  override def handleEnum(e: Enum): Result[List[Doc]] =
    List(mkEnumSchema(e), mkEnumValidator(e)).sequence

  override def handleADT(a: ADT): Result[List[Doc]] =
    List(mkAdtSchema(a), mkDerivedValidator(a)).sequence
}
