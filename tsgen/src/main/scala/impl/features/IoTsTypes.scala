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

  private val genTypeConstName: String => Doc = n =>
    Doc.text(s"${cfg.typePrefix}${n}${cfg.typeEnding}")

  private def genIotsTypeRef(ref: TypeRef): Doc =
    cfg
      .typeMapping(cfg)
      .lift(ref)
      .map(Doc.text)
      .getOrElse(
        TypeRef.cata(
          s => {
            val c = genTypeConstName(s.asString)
            c + (s.parameters match {
              case Nil => Doc.empty
              case l =>
                Doc.char('(') + Doc.intercalate(
                  Doc.text(", "),
                  l.map(genIotsTypeRef)
                ) + Doc.char(')')
            })
          },
          g => genTypeConstName(g.asString)
        )(ref)
      )

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
              .text(s": ${iots}Type<${f.`type`.name.name}>")
        )
      ) +
      Doc.text(") => ")
  }

  private def genTypeConstType(ref0: TypeRef,
                               tag: Option[String],
                               params: List[GenericDef]): Doc = {
    val typePrefix = tag.fold("Type<")(t => s"""Tagged<"$t", """)
    val ref = Doc.text(s"${iots}${typePrefix}") + createTypeRef(ref0) + Doc
      .char('>')
    params.toNel.fold(ref)(l => genIotsGenericPrefix(l) + ref)
  }

  private def genTypeConstPrefix(name: Ident,
                                 tag: Option[String],
                                 parameters: List[GenericDef],
                                 ref: TypeRef): Doc =
    exported(Doc.text("const ")) +
      genTypeConstName(name.name) +
      Doc.text(": ") + genTypeConstType(ref, tag, parameters) +
      Doc.text(" = ") +
      parameters.toNel.fold(Doc.empty)(genIotsGenericPrefix)

  private def genAliasOrNewtype(name: Ident,
                                parameters: List[GenericDef],
                                ref: TypeRef,
                                baseType: TypeRef): Doc =
    genTypeConstPrefix(name, None, parameters, ref) +
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

  override def handlePackage(p: Package) = ok(
    p.types
      .find(t => t.isRecord || t.isADT)
      .map { _ =>
        Doc.text(
          s"""|const typeImpl = <R extends ${iots}Props, O extends ${iots}Props>(r: R, o: O, name: string) =>
            |  ${iots}intersection([${iots}interface(r), ${iots}partial(o)], name)""".stripMargin
        )
      }
      .toList ++ p.types
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

  private def genRecordTypeConst(opt: Option[OptionalEncoding], indent: Int)(
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
    genTypeConstPrefix(name, tag, parameters, ref) + genRecordTypeConstImpl(
      name,
      reqf,
      optf,
      indent
    )
  }

  override def handleRecord(r: Record) =
    for {
      opt <- getOptionalEncoding
      indent <- getIndent
    } yield
      List(
        genRecordTypeConst(opt, indent)(
          r.name,
          None,
          r.parameters,
          Nil,
          r.fields.toList,
          r.ref
        )
      )

  private def genADTTaggedUnionImpl(indent: Int,
                                    tagName: String,
                                    genAdtNs: Boolean)(a: ADT): Doc = {
    val prefix = Doc.text(s"""${iots}taggedUnion("${tagName}", [""")
    Doc
      .intercalate(Doc.char(',') + Doc.lineOrSpace, a.constructors.toList.map {
        c =>
          val typ = genIotsTypeRef(c.ref)
          if (genAdtNs) Doc.text(a.name.name) + Doc.char('.') + typ else typ
      })
      .tightBracketBy(prefix, Doc.text(s"""], "${a.name.name}")"""), indent)
  }

  private def genADTTypeConst(indent: Int, tagName: String, genAdtNs: Boolean)(
    a: ADT
  ): Doc = {
    genTypeConstPrefix(a.name, None, a.parameters, a.ref) +
      genADTTaggedUnionImpl(indent, tagName, genAdtNs)(a)
  }

  override def handleADT(a: ADT) =
    for {
      opt <- getOptionalEncoding
      indent <- getIndent
      tagName <- getAdtTag
      genAdtNs <- getGenerateAdtNs
      constructors0 = a.constructors.toList.map { c =>
        val ref = c.ref
        val tag = Doc.text(c.name.name)
        val tagExpr = Doc.text(s"""$tagName: t.literal("""") + tag + Doc.text(
          """")"""
        )
        genRecordTypeConst(opt, indent)(
          c.name,
          tagName.some,
          a.parameters,
          List(tagExpr),
          c.fields,
          ref
        )
      }
      constructors = genAdtNs match {
        case false => constructors0
        case _ => List(withNamespace(a.name.name, indent)(constructors0))
      }
    } yield constructors ++ List(genADTTypeConst(indent, tagName, genAdtNs)(a))

  override def handleEnum(e: Enum) =
    for {
      indent <- getIndent
      name = e.name.name
    } yield
      List(
        genTypeConstPrefix(e.name, None, Nil, e.ref) +
          Doc.text(s"""stringEnumImpl<$name>($name, "$name")""")
      )
}
