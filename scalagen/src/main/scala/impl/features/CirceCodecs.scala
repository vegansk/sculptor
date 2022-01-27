package sculptor.scalagen
package impl
package features

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._

final class CirceCodecs(adtTag: String) extends Feature with GenHelpers {

  import ScalaIdent._

  private def genNames(encType: Ident,
                       params0: List[GenericDef],
                       objEncoder: Boolean) = {
    val params1 = params0.map(p => Doc.text(p.`type`.name.asScalaId))
    val params = createParameters0(params1)
    val paramsEnc = createParameters0(params1, Doc.text(":Encoder"))
    val paramsDec = createParameters0(params1, Doc.text(":Decoder"))
    val (enc0, dec0) = params0.toNel.fold(
      (
        Doc.text(s"implicit val ${encType.name}Encoder"),
        Doc.text(s"implicit val ${encType.name}Decoder")
      )
    ) { l =>
      (
        Doc.text(s"implicit def ${encType.name}Encoder") + paramsEnc,
        Doc.text(s"implicit def ${encType.name}Decoder") + paramsDec
      )
    }
    (
      enc0 + Doc.text(
        s""": ${if (objEncoder) "Encoder.AsObject" else "Encoder"}[${encType.asScalaId}"""
      ) + params + Doc.char(']'),
      dec0 + Doc.text(s": Decoder[${encType.asScalaId}") + params + Doc.char(
        ']'
      )
    )
  }

  override def handleNewtype(n: Newtype) = {
    val (enc, dec) = genNames(n.name, n.parameters, false)
    val valType = createTypeRef(n.baseType)
    ok(
      List(
        enc + Doc.text(" = Encoder[") + valType + Doc
          .text("].contramap(_.value)"),
        dec + Doc.text(" = Decoder[") + valType + Doc
          .text(s"].map(${n.name.asScalaId}(_))")
      )
    )
  }

  override def handleEnum(e: Enum) = {
    val name = e.name.asScalaId
    val (enc, dec) = genNames(e.name, Nil, false)
    ok(
      List(
        enc + Doc.text(s" = Encoder[String].contramap($name.asString(_))"),
        dec + Doc.text(
          s""" = Decoder[String].emap(v => $name.fromString.lift(v).toRight("Invalid enum value $name." + v))"""
        )
      )
    )
  }

  private lazy val tagName: String =
    if (adtTag.isEmpty) "__tag" else adtTag

  private def genRecordEncoderBody(objName: String,
                                   tag0: Option[String],
                                   fields: List[FieldDef],
                                   indent: Int): Doc = {
    val prefix = Doc.text("JsonObject(")
    val postfix = caseClassPostfix
    val tag = tag0.toList.map(t => Doc.text(s""""${tagName}" := "$t""""))
    bracketBy(
      Doc
        .intercalate(
          Doc.char(',') + line,
          tag ++
            fields.map(
              f =>
                Doc
                  .text(s""""${f.name.name}" := $objName.${f.name.asScalaId}""")
            )
        )
    )(prefix, postfix, indent)
  }

  private def genRecordDecoderBody(typeName: Ident,
                                   tag: Option[String],
                                   params0: List[GenericDef],
                                   cursorName: String,
                                   fields: List[FieldDef],
                                   indent: Int): Doc = {
    val prefix = Doc.text("for {")
    val params = createParameters(params0)
    val postfix = objectPostfix +
      bracketBy(
        Doc
          .intercalate(
            Doc.char(',') + Doc.lineOrSpace,
            fields.map(f => Doc.text(f.name.asScalaId))
          )
      )(
        Doc.text(s" yield ${typeName.asScalaId}") + params + Doc.text("("),
        caseClassPostfix,
        indent
      )
    bracketBy(
      Doc
        .intercalate(
          line,
          fields.map(
            f =>
              Doc.text(
                s"""${f.name.asScalaId} <- $cursorName.downField("${f.name.name}").as["""
              ) + createTypeRef(f.`type`) + Doc.char(']')
          )
        )
    )(prefix, postfix, indent)
  }

  override def handleRecord(r: Record) =
    for {
      indent <- getIndent
      typ = createTypeRef(r.ref)
      (enc, dec) = genNames(r.name, r.parameters, true)
      encPrefix = enc + Doc.text(s" = Encoder.AsObject.instance[") + typ + Doc
        .text("] { v =>")
      encoder = bracketBy(
        genRecordEncoderBody("v", None, r.fields.toList, indent)
      )(encPrefix, objectPostfix, indent)
      decPrefix = dec + Doc.text(s" = Decoder.instance[") + typ + Doc.text(
        "] { c =>"
      )
      decoder = bracketBy(
        genRecordDecoderBody(
          r.name,
          None,
          r.parameters,
          "c",
          r.fields.toList,
          indent
        )
      )(decPrefix, objectPostfix, indent)
      result = List(encoder, decoder)
    } yield result

  private def isCaseObject(adt: ADT, cons: ADTConstructor): Boolean =
    cons.fields.isEmpty &&
      cons.parameters.isEmpty &&
      adt.parameters.isEmpty

  private def genADTEncoderBody(a: ADT, indent: Int) =
    Doc.intercalate(
      line,
      a.constructors.toList.map { c =>
        val tagValue = c.tag.getOrElse(c.name.name)
        val consRef = createTypeExpr(c.name, c.parameters)
        val pattern = if (c.fields.nonEmpty) {
          Doc.text("v: ") + consRef
        } else if (!isCaseObject(a, c)) {
          Doc.text("_: ") + consRef
        } else {
          consRef
        }
        Doc.text("case ") + pattern + Doc.text(" => ") +
          genRecordEncoderBody("v", tagValue.some, c.fields, indent)
      }
    )

  private def genADTDecoderBody(a: ADT, cursorName: String, indent: Int) = {
    val prefix =
      Doc.text(s"""$cursorName.downField("$tagName").as[String].flatMap {""")
    val postfix = objectPostfix
    bracketBy(
      Doc
        .intercalate(
          line,
          a.constructors.toList.map { c =>
            val tagValue = c.tag.getOrElse(c.name.name)
            val consRef = createTypeExpr(c.name, c.parameters)
            val prefix0 = Doc.text(s"""case "$tagValue" => """)
            if (isCaseObject(a, c)) {
              prefix0 + Doc.text(s"Right(") + consRef + Doc.text(")")
            } else if (c.fields.isEmpty)
              prefix0 + Doc.text(s"Right(") + consRef + Doc.text("())")
            else
              prefix0 + genRecordDecoderBody(
                c.name,
                tagValue.some,
                c.parameters,
                cursorName,
                c.fields,
                indent
              )
          } ++ List(
            Doc.text(
              s"""case tag => Left(DecodingFailure("Invalid ADT tag value ${a.name.name}." + tag, $cursorName.history))"""
            )
          )
        )
    )(prefix, postfix, indent)
  }

  override def handleADT(a: ADT) =
    for {
      indent <- getIndent
      typ = createTypeRef(a.ref)
      (enc, dec) = genNames(a.name, a.parameters, true)
      encPrefix = enc + Doc.text(s" = Encoder.AsObject.instance[") + typ + Doc
        .text("] {")
      encoder = bracketBy(genADTEncoderBody(a, indent))(
        encPrefix,
        objectPostfix,
        indent
      )
      decPrefix = dec + Doc.text(s" = Decoder.instance[") + typ + Doc.text(
        "] { c =>"
      )
      decoder = bracketBy(genADTDecoderBody(a, "c", indent))(
        decPrefix,
        objectPostfix,
        indent
      )
      result = List(encoder, decoder)
    } yield result

}
