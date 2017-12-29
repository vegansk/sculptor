package sculptor
package scalagen
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

  def comment(c: Option[Comment]): List[Doc] =
    c.filter(_ => config.parameters.generateComments)
      .map(_.replace("\n", " "))
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

  def mkInstanceVal(t: TypeRef, typeClass: String): Doc =
    spread(
      List(
        text("implicit val"),
        typeName(t, false) + text(typeClass) + char(':'),
        text(typeClass) + char('[') + typeName(t) + char(']'),
        eqSign
      )
    ) + space

  def mkInstanceByCall(t: TypeRef, typeClass: String, call: String): Doc =
    text(typeClass) + char('[') + typeName(t) + text("].") + text(call)

  def catsEqInstance(t: TypeRef): Option[Doc] =
    config.parameters.generateCatsEq match {
      case true =>
        Option(mkInstanceVal(t, "Eq") + text("Eq.fromUniversalEquals"))
      case _ => None
    }

  def createElem(elemNameVar: String, data: Doc): Doc =
    bracketBy(
      intercalate(
        comma + line,
        List(
          text("null"),
          text(mask(elemNameVar, true)),
          text("Null"),
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
        bracketBy(intercalate(comma + line, config.externalTypes.map { t =>
          xmlSerializerName(t.name) + text(s": String => ${t.name} => Node")
        }))(prefix, postfix).some
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
            createElem(elemName, text(s"Text($varName.toString)"))
          case _ =>
            text(s"$name.toXml(s)(") + text(elemName) + text(s")($varName)")
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
            text("Node = {")
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
      .map(t => prefix.fold(t)(p => text(p + ".") + t))
      .map(t => if (isArray) array(t) else t)
      .map(t => if (isOptional) optional(t) else t)

  /**
    * @param strongTypeExpr - always generate strong type expression
    */
  def typeExpr(f: FieldDecl,
               strongTypeExpr: Boolean = false,
               requiredField: Boolean = false): Doc =
    typeExprImpl(
      f,
      f.array,
      isOptionalField(f) || (!requiredField && !strongTypeExpr && config.parameters.generateOptionalTypes.toBool),
      config.parameters.generateOptionalTypes.strongTypesPrefix
        .filter(_ => strongTypeExpr)
    )

  def fieldDecl(f: FieldDecl, requiredField: Boolean = false): Doc =
    spread(
      List(
        ident(f.name) + char(':'),
        typeExpr(f, requiredField = requiredField)
      ) ++ comment(f.comment)
    )

  def complexTypeClassDecl(ct: ComplexTypeDecl): Doc = {
    val prefix = spread(List(text("final case class"), ident(ct.`type`.name))) + char(
      '('
    )
    val postfix = char(')')

    bracketBy(
      intercalate(
        comma + line,
        ct.fields.toList.map(f => fieldDecl(f, isRequiredField(ct.`type`, f)))
      )
    )(prefix, postfix)
  }

  def complexTypeCirceEncoder(ct: ComplexTypeDecl): Doc = {
    val prefix = mkInstanceVal(ct.`type`, "ObjectEncoder") + text(
      s"ObjectEncoder.instance[${typeNameString(ct.`type`)}] { v =>"
    )
    val postfix = char('}')

    val fields = ct.fields.toList.map { f =>
      spread(List(strLit(f.name.value), text(":= v.") + ident(f.name)))
    }

    val obj =
      bracketBy(intercalate(comma + line, fields))(
        text("JsonObject("),
        char(')')
      )

    bracketBy(obj)(prefix, postfix)
  }

  def complexTypeCirceDecoder(ct: ComplexTypeDecl): Doc = {
    val prefix = mkInstanceVal(ct.`type`, "Decoder") + text(
      s"Decoder.instance[${typeNameString(ct.`type`)}] { c =>"
    )
    val postfix = char('}')

    val fieldsDecls = ct.fields.toList.map { f =>
      spread(
        List(
          ident(f.name),
          text("<-"),
          text("c.downField(") + strLit(f.name.value) + text(").as[") + typeExpr(
            f,
            false,
            isRequiredField(ct.`type`, f)
          ) + char(']')
        )
      )
    }

    val fieldsList =
      intercalate(comma + space, ct.fields.toList.map(f => ident(f.name)))

    val `for` = bracketBy(stack(fieldsDecls))(text("for {"), char('}')) +
      space + text("yield") + space +
      typeName(ct.`type`) + char('(') + fieldsList + char(')')

    bracketBy(`for`)(prefix, postfix)
  }

  def complexTypeCirceCodecs(ct: ComplexTypeDecl): List[Doc] =
    config.parameters.generateCirceCodecs match {
      case true =>
        List(complexTypeCirceEncoder(ct), complexTypeCirceDecoder(ct))
      case false => Nil
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
        text("ct.") + ident(f.name) + text("toList.flatMap(v => ") +
          callXmlSerializer(strLitString(f.xmlName), "v", f.`type`) +
          char(')')
    }
  }

  def complexTypeXmlSerializer(ct: ComplexTypeDecl): Option[Doc] =
    config.parameters.generateXmlSerializers match {
      case true =>
        mkXmlSerilaizerFunc(
          ct.`type`,
          "ct",
          createElem(
            "elemName",
            intercalate(
              text(" ++") + line,
              ct.fields.toList.map(complexTypeFieldXmlSerializer)
            ) + text(":_*")
          )
        )
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

  def complexTypeOptionalTypeToStrongTypeConverter(
    ct: ComplexTypeDecl
  ): Option[Doc] =
    config.parameters.generateOptionalTypes match {
      case OptionalTypes.Generate(Some(sp), _) => {
        val prefix = mkOptionalTypeConverterPrefix(ct.`type`, sp)
        val postfix = char('}')

        val fields = ct.fields
          .map(
            f =>
              complexTypeOptionalTypeToStrongTypeFieldConverter(
                ct.`type`,
                f,
                sp
            )
          )
        val cvt =
          if (ct.fields.size === 1) {
            fields.head +
              text(".map(" + sp + ".") +
              typeName(ct.`type`) +
              text(".apply _)")
          } else {
            bracketBy(intercalate(comma + line, fields.toList))(
              char('('),
              char(')')
            ) +
              text(".mapN(" + sp + ".") +
              typeName(ct.`type`) +
              text(".apply _)")
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
          complexTypeOptionalTypeToStrongTypeConverter(ct).toList
      )
      .map(_.toList)

    elements.map { el =>
      val prefix = text("object ") + typeName(ct.`type`) + text(" {")
      val postfix = char('}')

      bracketBy(intercalate(line * 2, el))(prefix, postfix)
    }
  }

  def complexTypeDecl(ct: ComplexTypeDecl): Doc =
    stack(
      comment(ct.comment) ++
        complexTypeClassDecl(ct).pure[List] ++
        complexTypeObjectDecl(ct).toList
    )

  def enumMemberDecl(e: EnumDecl)(m: EnumMemberDecl): Doc = {
    val code = spread(List(text("override val code ="), strLit(m.value)))
    val description = spread(
      List(
        text("override val description ="),
        char('"') + text(m.comment.getOrElse(m.value)) + char('"')
      )
    )
    val prefix = spread(
      List(
        text("object"),
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
          mkInstanceVal(e.`type`, "Encoder") + mkInstanceByCall(
            TypeRef.definedFrom("String"),
            "Encoder",
            "contramap(_.code)"
          ),
          mkInstanceVal(e.`type`, "Decoder") + mkInstanceByCall(
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
      createElem("elemName", text("Text(e.code)"))
    )

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
          enumOptionalTypeToStrongTypeConverter(e).toList
      )
    )(prefix, postfix)
  }

  def enumTypeDecl(e: EnumDecl): Doc = {
    val prefix = spread(
      List(text("sealed"), text("trait"), typeName(e.`type`), char('{'))
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
          mkInstanceVal(t.`type`, "Encoder") + mkInstanceByCall(
            t.baseType,
            "Encoder",
            "contramap(_.value)"
          ),
          mkInstanceVal(t.`type`, "Decoder") + mkInstanceByCall(
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
