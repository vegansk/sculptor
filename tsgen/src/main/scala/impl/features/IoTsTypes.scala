package sculptor.tsgen
package impl
package features

import org.typelevel.paiges._
import cats.data.NonEmptyList
import cats.implicits._

import sculptor.ast._
import sculptor.tsgen.{Feature => TsFeature, _}

final case class IoTsTypes(cfg: TsFeature.IoTsTypes)
    extends Feature
    with GenHelpers {

  private val iots = if (cfg.iotsNs.isEmpty) "" else s"${cfg.iotsNs}."

  private val genTypeConstName: String => String = n =>
    s"${cfg.typePrefix}${n}${cfg.typeEnding}"

  private def genIotsTypeRef(ref: TypeRef): Doc =
    TypeRef.cata(
      s => {
        val c = Doc.text(
          cfg.typeMapping
            .lift((s.asString, Feature.IoTsTypes.iotsNsPrefix(cfg)))
            .getOrElse(genTypeConstName(s.asString))
        )
        c + (s.parameters match {
          case Nil => Doc.empty
          case l =>
            Doc.char('(') + Doc.intercalate(
              Doc.text(", "),
              l.map(genIotsTypeRef)
            ) + Doc.char(')')
        })
      },
      g => Doc.text(genTypeConstName(g.asString))
    )(ref)

  private def genIotsGenericPrefix(params: NonEmptyList[GenericDef]): Doc = {
    val l = params.toList
    Doc.char('<') +
      Doc.intercalate(Doc.char(','), l.map(f => Doc.text(f.`type`.name.name))) +
      Doc.text(">(") +
      Doc.intercalate(
        Doc.text(", "),
        l.map(
          f =>
            genIotsTypeRef(f.`type`) + Doc
              .text(s": ${iotsType}<${f.`type`.name.name}>")
        )
      ) + Doc.char(')')
  }

  private lazy val iotsType = cfg.customIotsType match {
    case "" => s"${iots}Type"
    case v => v
  }

  private lazy val iotsTaggedType = cfg.customIotsTaggedType match {
    case "" => s"${iots}Tagged"
    case v => v
  }

  private def genTypeConstType(ref0: TypeRef,
                               tag: Option[String],
                               params: List[GenericDef],
                               generateField: Boolean): Doc = {
    val typePrefix =
      tag.fold(s"${iotsType}<")(t => s"""${iotsTaggedType}<"$t", """)
    val ref = Doc.text(typePrefix) + createTypeRef(ref0) + Doc
      .char('>')
    if (generateField) ref
    else
      params.toNel.fold(ref)(
        l => genIotsGenericPrefix(l) + Doc.text(" => ") + ref
      )
  }

  private def genTypeConstPrefix(name: Ident,
                                 tag: Option[String],
                                 parameters: List[GenericDef],
                                 ref: TypeRef,
                                 generateExport: Boolean,
                                 generateField: Boolean): Doc = {
    if (generateField) {
      Doc.text(genTypeConstName(name.name)) + Doc.text(": ") +
        parameters.toNel.fold(Doc.empty)(
          genIotsGenericPrefix(_) +
            Doc.text(": ") + genTypeConstType(ref, tag, parameters, true) + Doc
            .text(" => ")
        )
    } else {
      val exported0: Doc => Doc =
        if (generateExport) exported(_) else identity[Doc](_)
      exported0(Doc.text("const ")) +
        Doc.text(genTypeConstName(name.name)) +
        Doc.text(": ") + genTypeConstType(ref, tag, parameters, false) +
        Doc.text(" = ") +
        parameters.toNel.fold(Doc.empty)(
          genIotsGenericPrefix(_) + Doc.text(" => ")
        )
    }
  }

  private def genTypeConstPostfix(tag: Option[String],
                                  parameters: List[GenericDef],
                                  ref: TypeRef,
                                  generateField: Boolean): Doc = {
    if (!generateField || parameters.length > 0) {
      // It's const definition or function field
      Doc.empty
    } else {
      // It's constant field
      Doc.text(" as ") + genTypeConstType(ref, tag, parameters, true)
    }
  }

  private def genAliasOrNewtype(name: Ident,
                                parameters: List[GenericDef],
                                ref: TypeRef,
                                baseType: TypeRef): Doc =
    genTypeConstPrefix(
      name,
      None,
      parameters,
      ref,
      generateExport = true,
      generateField = false
    ) +
      Doc.text("<any>") +
      genIotsTypeRef(baseType)

  override def handleAlias(a: Alias) = ok(
    List(genAliasOrNewtype(a.name, a.parameters, a.ref, a.baseType))
  )

  override def handleNewtype(n: Newtype) = ok(
    List(genAliasOrNewtype(n.name, n.parameters, n.ref, n.baseType))
  )

  private def processOptionalRequiredFields[A](opt: Option[OptionalEncoding])(
    required: List[FieldDef] => A,
    optional: List[FieldDef] => A
  )(l: List[FieldDef]): (A, A) = {
    val (reql, optl) = l.map(processField(opt)).partition(!_._1)
    (required(reql.map(_._2)), optional(optl.map(_._2)))
  }

  override def handlePackage(p: Package) = p.sortedTypes match {
    case Right(types) =>
      ok(
        types
          .find(t => t.isRecord || t.isADT)
          .map { _ =>
            Doc.text(
              s"""|const typeImpl = <R extends ${iots}Props, O extends ${iots}Props>(r: R, o: O, name: string) =>
                |  ${iots}intersection([${iots}interface(r), ${iots}partial(o)], name)""".stripMargin
            )
          }
          .toList ++ types
          .find(_.isEnum)
          .map { _ =>
            Doc.text(s"""|const getStringEnumValues = (o: object): string[] =>
                  |  Object.keys(o).map(_ => (o as { [n: string]: any })[_]).filter(v => typeof v === "string")
                  |
                  |const stringEnumImpl = <E>(e: object, name: string): ${iots}Type<E> => {
                  |  const values = getStringEnumValues(e)
                  |  return new ${iots}Type<E>(
                  |    name,
                  |    (v): v is E => values.indexOf(v as string) >= 0,
                  |    (v, c) => values.indexOf(v as string) >= 0 ? ${iots}success<E>(v as E) : ${iots}failure<E>(v, c),
                  |    ${iots}identity
                  |  )
                  |}""".stripMargin)
          }
          .toList
      )
    case Left(err) => error(err)
  }

  private def genFields(indent: Int,
                        append: List[Doc] = Nil)(l: List[FieldDef]): Doc =
    Doc
      .intercalate(
        Doc.char(',') + Doc.lineOrSpace,
        append ++ l.toList
          .map(f => Doc.text(s"${f.name.name}: ") + genIotsTypeRef(f.`type`))
      )
      .tightBracketBy(Doc.char('{'), Doc.char('}'), indent)

  private def genRecordTypeConstImpl(name: Ident,
                                     reqFields: Doc,
                                     optFields: Doc,
                                     indent: Int): Doc = {
    val prefix = Doc.text("typeImpl(")
    Doc
      .intercalate(
        Doc.char(',') + Doc.lineOrSpace,
        List(reqFields, optFields, Doc.text(s""""${name.name}""""))
      )
      .tightBracketBy(prefix, Doc.char(')'), indent)
  }

  // If generateField === true, generates field for codecs object
  private def genRecordTypeConst(opt: Option[OptionalEncoding],
                                 indent: Int,
                                 generateExport: Boolean,
                                 generateField: Boolean)(
    name: Ident,
    tag: Option[String],
    parameters: List[GenericDef],
    fieldsPrefix: List[Doc],
    fields: List[FieldDef],
    ref: TypeRef
  ): Doc = {
    val (reqf, optf) = processOptionalRequiredFields(opt)(
      genFields(indent, fieldsPrefix),
      genFields(indent)
    )(fields)
    genTypeConstPrefix(
      name,
      tag,
      parameters,
      ref,
      generateExport,
      generateField
    ) + genRecordTypeConstImpl(name, reqf, optf, indent) + genTypeConstPostfix(
      tag,
      parameters,
      ref,
      generateField
    )
  }

  override def handleRecord(r: Record) =
    for {
      opt <- getOptionalEncoding
      indent <- getIndent
    } yield
      List(
        genRecordTypeConst(
          opt,
          indent,
          generateExport = true,
          generateField = false
        )(r.name, None, r.parameters, Nil, r.fields.toList, r.ref)
      )

  private def genADTTaggedUnionImpl(indent: Int,
                                    tagName: String,
                                    genAdtNs: Boolean)(a: ADT): Doc = {
    if (a.constructors.size === 1) {
      val typ = genIotsTypeRef(a.constructors.head.ref)
      if (genAdtNs) Doc.text(s"${a.name.name}${cfg.codecsObjectEnding}.") + typ
      else typ
    } else {
      val prefix = Doc.text(s"${iots}union([")
      Doc
        .intercalate(
          Doc.char(',') + Doc.lineOrSpace,
          a.constructors.toList.map { c =>
            val typ = genIotsTypeRef(c.ref)
            if (genAdtNs)
              Doc.text(s"${a.name.name}${cfg.codecsObjectEnding}.") + typ
            else typ
          }
        )
        .tightBracketBy(prefix, Doc.text(s"""], "${a.name.name}")"""), indent)
    }
  }

  private def genADTTypeConst(indent: Int, tagName: String, genAdtNs: Boolean)(
    a: ADT
  ): Doc = {
    genTypeConstPrefix(
      a.name,
      None,
      a.parameters,
      a.ref,
      generateExport = true,
      generateField = false
    ) +
      genADTTaggedUnionImpl(indent, tagName, genAdtNs)(a)
  }

  private def withCodec(a: ADT, indent: Int, generateExport: Boolean)(
    what: List[Doc]
  ): Doc = {
    val codec = Doc
      .intercalate(Doc.char(',') + dblLine, what)
      .tightBracketBy(
        Doc.text(s"const ${a.name.name}${cfg.codecsObjectEnding} = {"),
        Doc.char('}'),
        indent
      )
    if (generateExport) exported(codec) else codec
  }

  private def adtConstructorTypeRef(a: ADT,
                                    c: ADTConstructor,
                                    genAdtNs: Boolean): TypeRef = {
    if (genAdtNs) {
      TypeRef.cata[TypeRef](
        s => s.copy(name = s.name.copy(prefix = a.name :: s.name.prefix)),
        identity
      )(c.ref)
    } else {
      c.ref
    }
  }

  override def handleADT(a: ADT) =
    for {
      opt <- getOptionalEncoding
      indent <- getIndent
      tagName <- getAdtTag
      genAdtNs <- getGenerateAdtNs
      constructors0 = a.constructors.toList.map { c =>
        val ref = adtConstructorTypeRef(a, c, genAdtNs)
        val tag = Doc.text(c.name.name)
        val tagExpr = Doc.text(s"""$tagName: t.literal("""") + tag + Doc.text(
          """")"""
        )
        genRecordTypeConst(
          opt,
          indent,
          generateExport = cfg.exportADTConstructorsCodecs,
          generateField = genAdtNs
        )(c.name, tagName.some, a.parameters, List(tagExpr), c.fields, ref)
      }
      constructors = genAdtNs match {
        case false => constructors0
        case _ =>
          List(
            withCodec(a, indent, cfg.exportADTConstructorsCodecs)(constructors0)
          )
      }
    } yield constructors ++ List(genADTTypeConst(indent, tagName, genAdtNs)(a))

  override def handleEnum(e: Enum) =
    for {
      indent <- getIndent
      name = e.name.name
    } yield
      List(
        genTypeConstPrefix(
          e.name,
          None,
          Nil,
          e.ref,
          generateExport = true,
          generateField = false
        ) +
          Doc.text(s"""stringEnumImpl<$name>($name, "$name")""")
      )
}
