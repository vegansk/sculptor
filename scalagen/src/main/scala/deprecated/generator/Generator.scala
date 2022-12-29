package sculptor.scalagen.deprecated
package generator

import cats._
import cats.data.{NonEmptyList => NEL}
import cats.implicits._
import org.typelevel.paiges._

class Generator(config: Config) {

  private def tabSize = 2

  import ast._

  import Doc._

  val dot: Doc = char('.')

  val eqSign: Doc = char('=')

  val asterisk: Doc = char('*')

  val const: Doc = text("const")

  val `type`: Doc = text("type")

  val `import`: Doc = text("import")

  def strLitString(v: String): String = "\"" + v + "\""

  def strLit(v: String): Doc = text(strLitString(v))

  private val reservedWords = Set(
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "forSome",
    "if",
    "implicit",
    "import",
    "lazy",
    "match",
    "new",
    "null",
    "object",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "super",
    "this",
    "throw",
    "trait",
    "try",
    "true",
    "type",
    "val",
    "var",
    "while",
    "with",
    "yield"
  )

  def mask(s: String, maskReservedWords: Boolean): String =
    maskReservedWords match {
      case true if reservedWords.contains(s) =>
        s"`$s`"
      case _ => s
    }

  def ident(i: Ident, maskReservedWords: Boolean = true): Doc =
    text(mask(i.value, maskReservedWords))

  def bracketBy(d: Doc)(left: Doc, right: Doc): Doc =
    left + (lineBreak + d).nested(tabSize) + lineBreak + right

  def qNameString(name: QName, maskReservedWords: Boolean = true): String =
    name.path.map(i => mask(i.value, maskReservedWords)).toList.mkString(".")

  def qName(name: QName, maskReservedWords: Boolean = true): Doc =
    text(qNameString(name, maskReservedWords))

  def array(`type`: Doc): Doc =
    text("List[") + `type` + char(']')

  def normalizeComment(c: String): String =
    c.split("\n")(0).replaceAll("\\*/", "\\*\\\\/")

  def comment(c: Option[Comment]): List[Doc] =
    c.filter(_ => config.parameters.generateComments)
      .map(normalizeComment)
      .fold[List[Doc]](Nil)(t => List(text("/** ") + text(t) + text(" */")))

  def nativeArray(`type`: Doc): Doc =
    `type` + text("[]")

  def optional(`type`: Doc): Doc =
    text("Option[") + `type` + char(']')

  def isOptionalField(f: FieldDecl): Boolean =
    List(
      FieldConstraint.Nullable,
      FieldConstraint.OptionalNullable,
      FieldConstraint.Optional
    ).contains(f.constraint)

  def isRequiredField(`type`: TypeRef.defined, f: FieldDecl): Boolean =
    !isOptionalField(f) && config.parameters.generateOptionalTypes.requiredFields
      .get(`type`.name.value)
      .fold(false)(_.contains(f.name.value))

  def typeNameString(t: TypeRef, maskReservedWords: Boolean = true): String =
    t match {
      case TypeRef.std(v) => mask(v.value, maskReservedWords)
      case TypeRef.defined(v) => mask(v.value, maskReservedWords)
      case TypeRef.external(v) => qNameString(v, maskReservedWords)
    }

  def typeName(t: TypeRef, maskReservedWords: Boolean = true): Doc =
    text(typeNameString(t, maskReservedWords))

  def mkInstanceVal(t: TypeRef, typeSuffix: String, typeClass: String): Doc =
    spread(
      List(
        text("implicit val"),
        typeName(t, false) + text(typeSuffix) + char(':'),
        text(typeClass) + char('[') + typeName(t) + char(']'),
        eqSign
      )
    ) + space

  def mkInstanceByCall(t: TypeRef, typeClass: String, call: String): Doc =
    text(typeClass) + char('[') + typeName(t) + text("].") + text(call)

  def catsEqInstance(t: TypeRef): Option[Doc] =
    config.parameters.generateCatsEq match {
      case true =>
        Option(mkInstanceVal(t, "Eq", "Eq") + text("Eq.fromUniversalEquals"))
      case _ => None
    }

  def createElem(elemNameVar: String, attr: Doc, data: Doc): Doc =
    bracketBy(
      intercalate(
        comma + line,
        List(
          text("null"),
          text(mask(elemNameVar, true)),
          attr,
          text("TopScope"),
          text("true"),
        ) ++ List(data)
      )
    )(text("Elem("), char(')'))

  val xmlSerializersClassName: Doc = text("XmlSerializers")

  val xmlSerializerParam: Doc =
    config.parameters.generateXmlSerializers && !config.externalTypes.isEmpty match {
      case true => {
        text("(s: ") + xmlSerializersClassName + char(')')
      }
      case _ => empty
    }

  def decapitalize(s: String): String = {
    val (x, xs) = s.splitAt(1)
    x.toLowerCase + xs
  }

  def xmlSerializerName(typeName: String): Doc =
    text(decapitalize(typeName) + "ToXml")

  def mkXmlSerializerUtils: Option[Doc] = {
    val code =
      """|private object utils {
          |  def unused[T](v: T): Unit = {
          |    val _ = v
          |  }
          |}""".stripMargin

    config.parameters.generateXmlSerializers && !config.externalTypes.isEmpty match {
      case true => text(code).some
      case _ => None
    }
  }

  def mkXmlSerializersObject: Option[Doc] =
    config.parameters.generateXmlSerializers && !config.externalTypes.isEmpty match {
      case true => {
        val prefix = text("final case class ") + xmlSerializersClassName + char(
          '('
        )
        val postfix = char(')')
        bracketBy(
          intercalate(
            comma + line,
            config.externalTypes.map(_.name).distinct.map { name =>
              xmlSerializerName(name) + text(s": String => ${name} => Elem")
            }
          )
        )(prefix, postfix).some
      }
      case _ => None
    }

  def callXmlSerializer(elemName: String,
                        varName: String,
                        `type`: TypeRef): Doc = {
    val name = typeNameString(`type`)
    config.externalTypes.find(_.name === name) match {
      case Some(_) =>
        text("s.") + xmlSerializerName(name) + text("(") + text(elemName) + text(
          s")($varName)"
        )
      case _ =>
        `type` match {
          case TypeRef.std(_) =>
            createElem(elemName, text("Null"), text(s"Text($varName.toString)"))
          case _ =>
            val serializers = if (!config.externalTypes.isEmpty) "(s)" else ""
            text(s"$name.toXml$serializers(") + text(elemName) + text(
              s")($varName)"
            )
        }
    }
  }

  def callFieldXmlSerializer(elemName: String,
                             objectName: String,
                             fieldName: String,
                             fieldType: TypeRef): Doc =
    callXmlSerializer(
      elemName,
      s"$objectName.${mask(fieldName, true)}",
      fieldType
    )

  def mkXmlSerializersObjectUnusedCall: Option[Doc] =
    config.parameters.generateXmlSerializers && !config.externalTypes.isEmpty match {
      case true => text("utils.unused(s)").some
      case _ => None
    }

  def mkXmlSerilaizerFunc(`type`: TypeRef,
                          varName: String,
                          code: => Doc): Option[Doc] =
    config.parameters.generateXmlSerializers match {
      case true => {
        val prefix = spread(
          List(
            text(s"def toXml") + xmlSerializerParam + text(
              s"(elemName: String)($varName:"
            ),
            typeName(`type`) + text("):"),
            text("Elem = {")
          )
        )
        val postfix = char('}')
        bracketBy(
          stack(
            mkXmlSerializersObjectUnusedCall.toList ++
              List(code)
          )
        )(prefix, postfix).some
      }
      case _ => None
    }

  def typeExprImpl(f: FieldDecl,
                   isArray: Boolean,
                   isOptional: Boolean,
                   prefix: Option[String]): Doc =
    (typeName(f.`type`): Id[Doc])
      .map(
        t =>
          prefix.filter(_ => f.`type`.isDefined).fold(t)(p => text(p + ".") + t)
      )
      .map(t => if (isArray) array(t) else t)
      .map(t => if (isOptional) optional(t) else t)

  /**
    * @param strongTypeExpr - always generate strong type expression
    */
  def typeExpr(f: FieldDecl,
               strongTypeExpr: Boolean = false,
               requiredField: Boolean = false,
               generateDefaultValue: Boolean = false): Doc = {
    val optionalField = isOptionalField(f) || (!requiredField && !strongTypeExpr && config.parameters.generateOptionalTypes.toBool)
    val defaultValue = if (generateDefaultValue && optionalField) {
      Some(text("= None"))
    } else {
      None
    }
    spread(
      List(
        typeExprImpl(
          f,
          f.array,
          optionalField,
          config.parameters.generateOptionalTypes.strongTypesPrefix
            .filter(_ => strongTypeExpr)
        ).some,
        defaultValue
      ).flattenOption
    )
  }

  def fieldDecl(f: FieldDecl,
                requiredField: Boolean,
                generateDefaultValue: Boolean = false): Doc = {
    spread(
      List(
        (ident(f.name) + char(':')),
        typeExpr(
          f,
          requiredField = requiredField,
          generateDefaultValue = generateDefaultValue
        )
      )
    )
  }

  def complexTypeClassDecl(ct: ComplexTypeDecl): Doc = {
    val prefix = spread(List(text("final case class"), ident(ct.`type`.name))) + char(
      '('
    )
    val postfix = char(')')

    bracketBy(
      intercalate(
        comma + line,
        ct.fields.toList.map(
          f =>
            fieldDecl(
              f,
              isRequiredField(ct.`type`, f),
              config.parameters.generateParametersDefaultValues
          )
        )
      )
    )(prefix, postfix)
  }

  def complexTypeCirceEncoderImpl(t: TypeRef.defined,
                                  fields0: List[FieldDecl]): Doc = {
    val prefix = mkInstanceVal(t, "Encoder", "Encoder.AsObject") + text(
      s"Encoder.AsObject.instance[${typeNameString(t)}] { v =>"
    )
    val postfix = char('}')

    val fields = fields0.map { f =>
      spread(List(strLit(f.name.value), text(":= v.") + ident(f.name)))
    }

    val obj =
      bracketBy(intercalate(comma + line, fields))(
        text("JsonObject("),
        char(')')
      )

    bracketBy(obj)(prefix, postfix)
  }

  def complexTypeCirceEncoder(ct: ComplexTypeDecl): Doc =
    complexTypeCirceEncoderImpl(ct.`type`, ct.fields.toList)

  def complexTypeCirceDecoderImpl(t: TypeRef.defined,
                                  fields: List[FieldDecl]): Doc = {
    val prefix = mkInstanceVal(t, "Decoder", "Decoder") + text(
      s"Decoder.instance[${typeNameString(t)}] { c =>"
    )
    val postfix = char('}')

    val fieldsDecls = fields.map { f =>
      spread(
        List(
          ident(f.name),
          text("<-"),
          text("c.downField(") + strLit(f.name.value) + text(").as[") + typeExpr(
            f,
            false,
            isRequiredField(t, f)
          ) + char(']')
        )
      )
    }

    val fieldsList =
      intercalate(comma + space, fields.map(f => ident(f.name)))

    val `for` = bracketBy(stack(fieldsDecls))(text("for {"), char('}')) +
      space + text("yield") + space +
      typeName(t) + char('(') + fieldsList + char(')')

    bracketBy(`for`)(prefix, postfix)
  }

  def complexTypeCirceDecoder(ct: ComplexTypeDecl): Doc =
    complexTypeCirceDecoderImpl(ct.`type`, ct.fields.toList)

  def complexTypeCirceCodecs(ct: ComplexTypeDecl): List[Doc] =
    config.parameters.generateCirceCodecs match {
      case true =>
        List(complexTypeCirceEncoder(ct), complexTypeCirceDecoder(ct))
      case false => Nil
    }

  def complexTypeAttrXmlSerializer(f: FieldDecl, objName: String): Doc = {
    val attrPrefix = text(s"Attribute(${strLitString(f.xmlName)}, ")
    val attrPostfix = text(".child, Null)")
    val attr = isOptionalField(f) match {
      case true =>
        text("ct.") + ident(f.name) + text(".map(v => ") + attrPrefix +
          callXmlSerializer("tmp", "v", f.`type`) + attrPostfix +
          text(").getOrElse(Null)")
      case false =>
        attrPrefix +
          callFieldXmlSerializer(""""tmp"""", objName, f.name.value, f.`type`) +
          attrPostfix
    }
    attr
  }

  def complexTypeAttrsXmlSerializer(fields: List[FieldDecl],
                                    objName: String): Doc = {
    text("Null") +
      fields
        .map(
          f =>
            text(".append(") + complexTypeAttrXmlSerializer(f, objName) + char(
              ')'
          )
        )
        .fold(empty)(_ + _)
  }

  def complexTypeFieldXmlSerializer(f: FieldDecl): Doc = {
    (isOptionalField(f), f.array) match {
      case (false, false) =>
        text("List(") + callFieldXmlSerializer(
          strLitString(f.xmlName),
          "ct",
          f.name.value,
          f.`type`
        ) + char(')')
      case (false, true) =>
        text("ct.") + ident(f.name) + text(".map(v => ") +
          callXmlSerializer(strLitString(f.xmlName), "v", f.`type`) +
          char(')')
      case (true, false) =>
        text("ct.") + ident(f.name) + text(".map(v => ") +
          callXmlSerializer(strLitString(f.xmlName), "v", f.`type`) +
          text(").toList")
      case (true, true) =>
        text("ct.") + ident(f.name) + text(".toList.flatten.flatMap(v => ") +
          callXmlSerializer(strLitString(f.xmlName), "v", f.`type`) +
          char(')')
    }
  }

  def complexTypeXmlSerializer(ct: ComplexTypeDecl): Option[Doc] = {
    val (attrs, fields) = ct.fields.toList.partition(_.attribute)
    config.parameters.generateXmlSerializers match {
      case true =>
        mkXmlSerilaizerFunc(
          ct.`type`,
          "ct",
          createElem(
            "elemName",
            complexTypeAttrsXmlSerializer(attrs, "ct"),
            intercalate(
              text(" ++") + line,
              fields.map(complexTypeFieldXmlSerializer)
            ) + text(":_*")
          )
        )
      case _ => None
    }
  }

  def complexTypeKantanFieldDecoder(f0: FieldDecl, nodeName: String): Doc = {
    val isOptionalList = f0.array && isOptionalField(f0)
    val f =
      if (isOptionalList) f0.copy(constraint = FieldConstraint.Required)
      else f0

    ident(f.name) + text(" <- Query[") + typeExpr(f) + text("](") +
      text(
        s"""xp"./${if (f.attribute) "@" + f.xmlName else f.xmlName}").eval(${nodeName})"""
      ) +
      (if (isOptionalList) text(".map(_.toNel.map(_.toList))") else empty)
  }

  def complexTypeKantanXPathDecoder(ct: ComplexTypeDecl): Option[Doc] =
    config.parameters.generateKantanXPathDecoders match {
      case true => {
        val fields = ct.fields.toList
        val fieldsList =
          intercalate(comma + space, fields.toList.map(f => ident(f.name)))
        val `for` = bracketBy(
          stack(fields.toList.map(complexTypeKantanFieldDecoder(_, "node")))
        )(
          text("for {"),
          text("} yield ") + typeName(ct.`type`) + char('(') + fieldsList + char(
            ')'
          )
        )
        val result = mkInstanceVal(ct.`type`, "NodeDecoder", "NodeDecoder") + bracketBy(
          `for`
        )(text("NodeDecoder.fromFound { node =>"), char('}'))
        result.some
      }
      case _ => None
    }

  def mkOptionalTypeConverterPrefix(t: TypeRef,
                                    strongTypesPrefix: String): Doc =
    text("def strong(v: ") +
      typeName(t) +
      text("): ValidatedNel[String, " + strongTypesPrefix + ".") +
      typeName(t) +
      text("] = {")

  def typeHasOptionalConverter(t: TypeRef): Boolean = t match {
    case _: TypeRef.defined => true
    case _ => false
  }

  def complexTypeOptionalTypeToStrongTypeFieldConverter(
    `type`: TypeRef.defined,
    f: FieldDecl,
    strongTypesPrefix: String
  ): Doc = {
    val field = text("v.") + ident(f.name)
    val isOptional = isOptionalField(f)
    val hasConverter = typeHasOptionalConverter(f.`type`)
    val isRequired = isRequiredField(`type`, f)
    val strong = f.array match {
      case true => text("v.traverse(") + typeName(f.`type`) + text(".strong _)")
      case _ => typeName(f.`type`) + text(".strong(v)")
    }
    val toValidNel = field + text(s""".toValidNel("${f.name.value}")""")
    val validNel = text("Validated.validNel[String,") + typeExpr(
      f,
      false,
      isRequired
    ) + text("](") + field + char(')')
    val appendPath = text(s""".leftMap(_.map("${f.name.value}." + _))""")
    val callCvt = text(".andThen(v => ") + strong + appendPath + text(")")
    val validStrongNelNone = text("Validated.validNel[String,") + typeExpr(
      f,
      true
    ) + text("](None)")
    val callOptionalCvt = text(".andThen(_.fold(") + validStrongNelNone + text(
      ")(v => "
    ) + strong + text(".map(_.some)") + appendPath + text("))")

    (isOptional, isRequired, hasConverter) match {
      case (false, true, false) => validNel
      case (false, true, true) => validNel + callCvt
      case (false, false, false) => toValidNel
      case (false, false, true) => toValidNel + callCvt
      case (true, _, false) => validNel
      case (true, _, true) => validNel + callOptionalCvt
    }
  }

  def createCurriedTypeConstructor(name: String,
                                   ct: ComplexTypeDecl,
                                   strongTypePrefix: String): Doc = {
    text(show"val $name = ") +
      intercalate(text(" => "), ct.fields.toList.map {
        case f =>
          char('(') + ident(f.name) + text(": ") + typeExpr(f, true) + char(')')
      }) +
      bracketBy(
        intercalate(text(", "), ct.fields.toList.map(f => ident(f.name)))
      )(
        text(show" => $strongTypePrefix.${typeNameString(ct.`type`)}("),
        char(')')
      )
  }

  def complexTypeOptionalTypeToStrongTypeConverter(
    ct: ComplexTypeDecl
  ): Option[Doc] =
    config.parameters.generateOptionalTypes match {
      case OptionalTypes.Generate(Some(strongTypePrefix), _) => {
        val prefix = mkOptionalTypeConverterPrefix(ct.`type`, strongTypePrefix)
        val postfix = char('}')

        val fields = ct.fields
          .map(
            f =>
              complexTypeOptionalTypeToStrongTypeFieldConverter(
                ct.`type`,
                f,
                strongTypePrefix
            )
          )
        val cvt =
          if (ct.fields.size === 1) {
            fields.head +
              text(".map(" + strongTypePrefix + ".") +
              typeName(ct.`type`) +
              text(".apply _)")
          } else {
            val cons =
              createCurriedTypeConstructor("cons", ct, strongTypePrefix)

            cons + line +
              intercalate(
                text(" <*>") + line,
                text("cons.validNel[String]") :: fields.toList
              )
          }

        bracketBy(cvt)(prefix, postfix).some
      }
      case _ => None
    }

  def complexTypeObjectDecl(ct: ComplexTypeDecl): Option[Doc] = {
    val elements = NEL
      .fromList(
        catsEqInstance(ct.`type`).toList ++
          complexTypeCirceCodecs(ct) ++
          complexTypeXmlSerializer(ct).toList ++
          complexTypeKantanXPathDecoder(ct).toList ++
          complexTypeOptionalTypeToStrongTypeConverter(ct).toList
      )
      .map(_.toList)

    elements.map { el =>
      val prefix = text("object ") + typeName(ct.`type`) + text(" {")
      val postfix = char('}')

      bracketBy(intercalate(line * 2, el))(prefix, postfix)
    }
  }

  def complexTypeComment(ct: ComplexTypeDecl): List[Doc] =
    config.parameters.generateComments match {
      case false => Nil
      case _ => {
        val comments = (
          ct.comment.map(normalizeComment) :: ct.fields.toList.map { f =>
            f.comment.map(
              c => show"@param ${f.name.value} ${normalizeComment(c)}"
            )
          }
        ).flattenOption.toNel
        comments match {
          case None => Nil
          case Some(NEL(x, xs)) =>
            text(show"/** $x") :: xs.map(c => text(show"  * $c")) ++ List(
              text("  */")
            )
        }
      }
    }

  def complexTypeDecl(ct: ComplexTypeDecl): Doc =
    stack(
      complexTypeComment(ct) ++
        complexTypeClassDecl(ct).pure[List] ++
        complexTypeObjectDecl(ct).toList
    )

  private val extensionContentFieldName = "baseContent"

  private def extensionContentField(`type`: TypeRef): FieldDecl = FieldDecl(
    Ident(extensionContentFieldName),
    "",
    `type`,
    FieldConstraint.Required,
    false,
    false,
    None
  )

  def simpleTypeExtensionClassDecl(t: SimpleTypeExtensionDecl): Doc = {
    val prefix = spread(List(text("final case class"), ident(t.`type`.name))) + char(
      '('
    )
    val postfix = char(')')

    bracketBy(
      intercalate(
        comma + line,
        (extensionContentField(t.baseType) :: t.fields.toList)
          .map(f => fieldDecl(f, isRequiredField(t.`type`, f)))
      )
    )(prefix, postfix)
  }

  def simpleTypeExtensionCirceCodecs(t: SimpleTypeExtensionDecl): List[Doc] =
    config.parameters.generateCirceCodecs match {
      case true => {
        val fields = extensionContentField(t.`type`) :: t.fields.toList
        List(
          complexTypeCirceEncoderImpl(t.`type`, fields),
          complexTypeCirceDecoderImpl(t.`type`, fields)
        )
      }
      case false => Nil
    }

  def simpleTypeExtensionXmlSerializer(
    t: SimpleTypeExtensionDecl
  ): Option[Doc] =
    config.parameters.generateXmlSerializers match {
      case true => {
        mkXmlSerilaizerFunc(
          t.`type`,
          "t",
          callFieldXmlSerializer(
            "elemName",
            "t",
            extensionContentFieldName,
            t.baseType
          ) +
            text(" % ") + complexTypeAttrsXmlSerializer(t.fields.toList, "t")
        )
      }
      case false => None
    }

  def simpleTypeExtensionKantanXPathDecoder(
    t: SimpleTypeExtensionDecl
  ): Option[Doc] = {
    config.parameters.generateKantanXPathDecoders match {
      case true => {
        val result = mkInstanceVal(t.`type`, "NodeDecoder", "NodeDecoder") + bracketBy(
          intercalate(
            comma + lineBreak,
            text("""xp"."""") ::
              t.fields.toList.map(f => text(s"""xp"./${"@" + f.xmlName}""""))
          )
        )(
          text("NodeDecoder.decoder("),
          text(s")(${typeNameString(t.`type`)}.apply)")
        )
        result.some
      }
      case _ => None
    }
  }

  // Issue #114: Assume here that we can't have more than 21 attributes in
  // complexType with simpleContent
  def simpleTypeExtensionOptionalTypeToStrongTypeConverter(
    t: SimpleTypeExtensionDecl
  ): Option[Doc] =
    config.parameters.generateOptionalTypes match {
      case OptionalTypes.Generate(Some(sp), _) => {
        val prefix = mkOptionalTypeConverterPrefix(t.`type`, sp)
        val postfix = char('}')

        val fields = (extensionContentField(t.`type`) :: t.fields)
          .map(
            f =>
              complexTypeOptionalTypeToStrongTypeFieldConverter(t.`type`, f, sp)
          )
        val cvt =
          if (fields.size === 1) {
            fields.head +
              text(".map(" + sp + ".") +
              typeName(t.`type`) +
              text(".apply _)")
          } else {
            bracketBy(intercalate(comma + line, fields.toList))(
              char('('),
              char(')')
            ) +
              text(".mapN(" + sp + ".") +
              typeName(t.`type`) +
              text(".apply _)")
          }

        bracketBy(cvt)(prefix, postfix).some
      }
      case _ => None
    }

  def simpleTypeExtensionObjectDecl(t: SimpleTypeExtensionDecl): Option[Doc] = {
    val elements = NEL
      .fromList(
        catsEqInstance(t.`type`).toList ++
          simpleTypeExtensionCirceCodecs(t) ++
          simpleTypeExtensionXmlSerializer(t).toList ++
          simpleTypeExtensionKantanXPathDecoder(t).toList ++
          simpleTypeExtensionOptionalTypeToStrongTypeConverter(t).toList
      )
      .map(_.toList)

    elements.map { el =>
      val prefix = text("object ") + typeName(t.`type`) + text(" {")
      val postfix = char('}')

      bracketBy(intercalate(line * 2, el))(prefix, postfix)
    }
  }

  def simpleTypeExtensionDecl(t: SimpleTypeExtensionDecl): Doc =
    stack(
      comment(t.comment) ++
        simpleTypeExtensionClassDecl(t).pure[List] ++
        simpleTypeExtensionObjectDecl(t).toList
    )

  def enumMemberDecl(e: EnumDecl)(m: EnumMemberDecl): Doc = {
    val code = spread(List(text("override val code ="), strLit(m.value)))
    val description = spread(
      List(
        text("override val description ="),
        char('"') + text(m.comment.getOrElse(m.value).split("\n")(0)) + char(
          '"'
        )
      )
    )
    val prefix = spread(
      List(
        text("case object"),
        ident(m.name),
        text("extends"),
        typeName(e.`type`),
        char('{')
      )
    )
    bracketBy(stack(List(code, description)))(prefix, char('}'))
  }

  def enumValuesVal(e: EnumDecl): Doc = {
    val prefix = text("lazy val values = Set[") + typeName(e.`type`) + text(
      "]("
    )
    val suffix = char(')')

    intercalate(
      (comma + line).grouped,
      e.members.toList.map(m => ident(m.name))
    ).tightBracketBy(prefix, suffix, tabSize)
  }

  def enumFromStringFunc(e: EnumDecl): Doc =
    bracketBy(text("s => values.find(_.code === s)"))(
      text("val fromString: String => Option[") +
        typeName(e.`type`) + text("] = {"),
      char('}')
    )

  def enumCirceCodecs(e: EnumDecl): List[Doc] =
    config.parameters.generateCirceCodecs match {
      case true =>
        List(
          mkInstanceVal(e.`type`, "Encoder", "Encoder") + mkInstanceByCall(
            TypeRef.definedFrom("String"),
            "Encoder",
            "contramap(_.code)"
          ),
          mkInstanceVal(e.`type`, "Decoder", "Decoder") + mkInstanceByCall(
            TypeRef.definedFrom("String"),
            "Decoder",
            "emap(fromString(_).toRight(\"Invalid enum value\"))"
          )
        )
      case _ => Nil
    }

  def enumXmlSerilaizer(e: EnumDecl): Option[Doc] =
    mkXmlSerilaizerFunc(
      e.`type`,
      "e",
      createElem("elemName", text("Null"), text("Text(e.code)"))
    )

  def enumKantanXPathDecoder(e: EnumDecl): Option[Doc] =
    config.parameters.generateKantanXPathDecoders match {
      case true => {
        val result = mkInstanceVal(e.`type`, "NodeDecoder", "NodeDecoder") +
          bracketBy(
            text("StringDecoder.fromPartial(Function.unlift(") +
              typeName(e.`type`) + text(".fromString).andThen(Right(_)))")
          )(text("codecs.fromString("), char(')'))
        result.some
      }
      case _ => None
    }

  def enumOptionalTypeToStrongTypeConverter(e: EnumDecl): Option[Doc] =
    config.parameters.generateOptionalTypes match {
      case OptionalTypes.Generate(Some(sp), _) => {
        val prefix = mkOptionalTypeConverterPrefix(e.`type`, sp)
        val postfix = char('}')
        val body = text(sp + ".") +
          typeName(e.`type`) +
          text(".fromString(v.code).toValidNel(v.code)")

        bracketBy(body)(prefix, postfix).some
      }
      case _ => None
    }

  def enumObjectDecl(e: EnumDecl): Doc = {
    val prefix = spread(List(text("object"), typeName(e.`type`), char('{')))
    val postfix = char('}')

    bracketBy(
      intercalate(line, e.members.map(enumMemberDecl(e) _).toList) + line * 2 + intercalate(
        line * 2,
        List(enumValuesVal(e), enumFromStringFunc(e)) ++
          catsEqInstance(e.`type`).toList ++
          enumCirceCodecs(e) ++
          enumXmlSerilaizer(e).toList ++
          enumKantanXPathDecoder(e).toList ++
          enumOptionalTypeToStrongTypeConverter(e).toList
      )
    )(prefix, postfix)
  }

  def enumTypeDecl(e: EnumDecl): Doc = {
    val prefix = spread(
      List(
        text("sealed"),
        text("trait"),
        typeName(e.`type`),
        text("extends"),
        text("Product"),
        text("with"),
        text("Serializable"),
        char('{')
      )
    )
    val suffix = char('}')

    bracketBy(
      stack(List(text("val code: String"), text("val description: String")))
    )(prefix, suffix)
  }

  def enumDecl(e: EnumDecl): Doc =
    stack(
      comment(e.comment) ++
        List(enumTypeDecl(e), enumObjectDecl(e))
    )

  def newtypeCirceCodecs(t: NewtypeDecl): List[Doc] =
    config.parameters.generateCirceCodecs match {
      case true =>
        List(
          mkInstanceVal(t.`type`, "Encoder", "Encoder") + mkInstanceByCall(
            t.baseType,
            "Encoder",
            "contramap(_.value)"
          ),
          mkInstanceVal(t.`type`, "Decoder", "Decoder") + mkInstanceByCall(
            t.baseType,
            "Decoder",
            s"map(${typeNameString(t.`type`)}(_))"
          )
        )
      case _ => Nil
    }

  def newtypeXmlSerializer(t: NewtypeDecl): Option[Doc] =
    mkXmlSerilaizerFunc(
      t.`type`,
      "t",
      callFieldXmlSerializer("elemName", "t", "value", t.baseType)
    )

  def newtypeKantanXPathDecoder(t: NewtypeDecl): Option[Doc] =
    config.parameters.generateKantanXPathDecoders match {
      case true => {
        val result = mkInstanceVal(t.`type`, "NodeDecoder", "NodeDecoder") +
          mkInstanceByCall(
            t.baseType,
            "NodeDecoder",
            s"map(${typeNameString(t.`type`)}(_))"
          )
        result.some
      }
      case _ => None
    }

  def newtypeOptionalTypeToStrongTypeConverter(t: NewtypeDecl): Option[Doc] = {
    config.parameters.generateOptionalTypes match {
      case OptionalTypes.Generate(Some(sp), _) => {
        val prefix = mkOptionalTypeConverterPrefix(t.`type`, sp)
        val postfix = char('}')
        val strongType = text(sp + ".") + typeName(t.`type`)
        val code = typeHasOptionalConverter(t.baseType) match {
          case true =>
            typeName(t.baseType) + text(".strong(v.value).map(") + strongType + text(
              ".apply _)"
            )
          case false =>
            text("Validated.validNel[String, ") + strongType + text("](") + strongType + text(
              "(v.value))"
            )
        }
        bracketBy(code)(prefix, postfix).some
      }
      case _ => None
    }
  }

  def newtypeObjectDecl(t: NewtypeDecl): Option[Doc] = {
    val elements = NEL
      .fromList(
        catsEqInstance(t.`type`).toList ++
          newtypeCirceCodecs(t) ++
          newtypeXmlSerializer(t).toList ++
          newtypeKantanXPathDecoder(t).toList ++
          newtypeOptionalTypeToStrongTypeConverter(t).toList
      )
      .map(_.toList)

    elements.map { xs =>
      val prefix = text("object ") + typeName(t.`type`) + text(" {")
      val postfix = char('}')
      bracketBy(intercalate(line * 2, xs))(prefix, postfix)
    }
  }

  def newtypeClassDecl(t: NewtypeDecl): Doc = {
    val prefix = text("final case class ") + typeName(t.`type`) + char('(')
    val postfix = char(')')

    bracketBy(text("value: ") + typeName(t.baseType))(prefix, postfix)
  }

  def newtypeDecl(t: NewtypeDecl): Doc =
    stack(
      comment(t.comment) ++
        newtypeClassDecl(t).pure[List] ++
        newtypeObjectDecl(t).toList
    )

  def typeDecl(t: TypeDecl): Doc = t match {
    case v: ComplexTypeDecl => complexTypeDecl(v)
    case v: EnumDecl => enumDecl(v)
    case v: NewtypeDecl => newtypeDecl(v)
    case v: SimpleTypeExtensionDecl => simpleTypeExtensionDecl(v)
  }

  def typesDecl(t: TypesDecl): Doc =
    intercalate(line + line, t.value.map(typeDecl _).toList)

  def importDecl(i: ImportDecl): Doc =
    spread(List(text("import"), text(i.path)))

  def importsDecl(i: ImportsDecl): Doc =
    stack(i.value.map(importDecl _).toList)

  def packageDecl: Option[Doc] =
    config.packageName.map { p =>
      text("package ") + text(p)
    }

  def moduleDecl(m: ModuleDecl): Doc =
    intercalate(
      line + line,
      config.header.map(text _).toList ++
        packageDecl.toList ++
        m.imports.map(importsDecl _).toList ++
        mkXmlSerializersObject.toList ++
        mkXmlSerializerUtils.toList ++
        m.types.map(typesDecl _).toList
    )
}
