package sculptor.xsd

import sculptor._
import types._
import scala.util.Try
import scala.xml._
import cats.implicits._

object parser {

  import helpers._

  private def parseTypeName(name: String): ResultS[TypeT] = {
    for {
      ns <- getNs
      pname <- liftE(prefixed(name))
      typ <- {
        pname match {
          case (ns1, name) if ns1 === ns => {
            name match {
              case "integer" => right(TypeT(IntegerF()))
              case "string" => right(TypeT(StringF()))
              case "decimal" => right(TypeT(DecimalF()))
              case "nonNegativeInteger" => right(TypeT(NonNegativeIntegerF()))
              case "byte" => right(TypeT(ByteF()))
              case "int" => right(TypeT(IntF()))
              case "long" => right(TypeT(LongF()))
              case "negativeInteger" => right(TypeT(NegativeIntegerF()))
              case "nonPositiveInteger" => right(TypeT(NonPositiveIntegerF()))
              case "positiveInteger" => right(TypeT(PositiveIntegerF()))
              case "short" => right(TypeT(ShortF()))
              case "unsignedLong" => right(TypeT(UnsignedLongF()))
              case "unsignedInt" => right(TypeT(UnsignedIntF()))
              case "unsignedShort" => right(TypeT(UnsignedShortF()))
              case "unsignedByte" => right(TypeT(UnsignedByteF()))
              case "date" => right(TypeT(DateF()))
              case _ => leftStr(s"Unknown xsd type $name")
            }
          }
          //TODO: Namespaces!!!
          case (_, name) => right(TypeT(TypeIdF(Ident(name))))
        }
      }
    } yield typ
  }

  private def parseAnonType(node: Node): ResultS[TypeT] = {
    node.child
      .find(_.label match {
        case "simpleType" | "complexType" => true
        case _ => false
      })
      .fold(leftStr[TypeT]("Can't parse anonymous type")) { v =>
        v.label match {
          case "simpleType" => parseSimpleType0(v)
          case "complexType" => parseComplexType0(v)
          case _ => leftStr(s"Unknown anonymous type ${v.label}")
        }
      }
  }

  private def parseElement(node: Node): ResultS[TypeT] = {
    withNode(node) {
      for {
        name <- attr("name")(node).map(Ident(_))
        typ <- attrO("type")(node).flatMap(
          t => t.fold(parseAnonType(node))(tname => parseTypeName(tname))
        )
      } yield TypeT(FieldF(name, typ))
    }
  }

  private def parseRestrictedString(name: Option[Ident],
                                    restriction: Node): ResultS[TypeT] = {
    withNode(restriction) {
      for {
        minLength <- optElAttrAsNumber("minLength", "value")(restriction)
        maxLength <- optElAttrAsNumber("maxLength", "value")(restriction)
        regExp <- els("pattern")(restriction)
          .flatMap(_.traverse(attr("value")(_)))
        rs <- right(
          RestrictedStringF(
            baseType = TypeT(StringF()),
            minLength = minLength,
            maxLength = maxLength,
            regExp = regExp
          ): TypeF[TypeT]
        )
      } yield TypeT(name.fold(rs)(NamedTypeF(_, rs)))
    }
  }

  private def parseRestrictedNumber(name: Option[Ident],
                                    baseType: TypeT,
                                    restriction: Node): ResultS[TypeT] = {
    withNode(restriction) {
      for {
        minInclusive <- optElAttrAsNumber("minInclusive", "value")(restriction)
        maxInclusive <- optElAttrAsNumber("maxInclusive", "value")(restriction)
        minExclusive <- optElAttrAsNumber("minExclusive", "value")(restriction)
        maxExclusive <- optElAttrAsNumber("maxExclusive", "value")(restriction)
        totalDigits <- optElAttrAsNumber("totalDigits", "value")(restriction)
        regExp <- els("pattern")(restriction)
          .flatMap(_.traverse(attr("value")(_)))
        rn <- right(
          RestrictedNumberF(
            baseType = baseType,
            minInclusive = minInclusive,
            maxInclusive = maxInclusive,
            minExclusive = minExclusive,
            maxExclusive = maxExclusive,
            totalDigits = totalDigits,
            regExp = regExp
          ): TypeF[TypeT]
        )
      } yield TypeT(name.fold(rn)(NamedTypeF(_, rn)))
    }
  }

  private def parseSimpleType0(node: Node): ResultS[TypeT] = {
    withNode(node) {
      for {
        name <- attrO("name")(node).map(_.map(Ident(_)))
        restriction <- el("restriction")(node)
        base <- attr("base")(restriction).flatMap(parseTypeName(_))
        `type` <- base match {
          case TypeT(StringF()) =>
            parseRestrictedString(name, restriction)
          case t @ TypeT(
                IntegerF() | DecimalF() | NonNegativeIntegerF() | ByteF() |
                IntF() | LongF() | NegativeIntegerF() | NonPositiveIntegerF() |
                PositiveIntegerF() | ShortF() | UnsignedLongF() |
                UnsignedIntF() | UnsignedShortF() | UnsignedByteF()
              ) =>
            parseRestrictedNumber(name, t, restriction)
          case typ => leftStr(s"Unknown base type: $typ")
        }
      } yield `type`
    }
  }

  private def parseSimpleType(node: Node): ResultS[TypeT] = {
    for {
      `type` <- parseSimpleType0(node)
      name <- {
        `type`.unfix match {
          case NamedTypeF(name, _) =>
            right(name)
          case _ => leftStr[Ident](s"Can't get the name of type ${`type`}")
        }
      }
    } yield `type`
  }

  private def parseSimpleTypes(root: Node): ResultS[List[TypeT]] =
    els("simpleType")(root).flatMap(_.traverse(parseSimpleType(_)))

  private def toComplexTypeF(
    f: List[TypeT] => SeqLike[TypeT]
  ): Node => ResultS[ComplexTypeF[TypeT]] =
    el =>
      for {
        body <- parseSeqBody(el)
        t <- right(ComplexTypeF(f(body)))
      } yield t

  private def parseSeqChild(el: Node): ResultS[TypeT] = {
    withNode(el) {
      // Use complexType body's fold here
      withComplexBody(
        // Else branch
        _.label match {
          case "element" => parseElement(el)
          case v => leftStr[TypeT](s"Unknown sequence child type $v")
        }
      )(
        el => toComplexTypeF(body => Sequence(body))(el).map(t => TypeT(t)),
        el => toComplexTypeF(body => Choice(body))(el).map(t => TypeT(t)),
        el =>
          leftStr[TypeT](s"Type all cannot be used inside another sequence")
      )(el)
    }
  }

  private def parseSeqBody(el: Node): ResultS[List[TypeT]] =
    xml
      .childElems(el)
      .map(parseSeqChild(_))
      .sequence

  private def parseComplexTypeBody(node: Node): ResultS[ComplexTypeF[TypeT]] =
    withComplexTypeS(
      toComplexTypeF(Sequence(_)),
      toComplexTypeF(Choice(_)),
      toComplexTypeF(All(_))
    )(node)

  private def parseComplexType0(node: Node): ResultS[TypeT] = {
    withNode(node) {
      for {
        name <- attrO("name")(node).map(_.map(Ident(_)))
        body <- parseComplexTypeBody(node)
      } yield TypeT(name.fold(body: TypeF[TypeT])(NamedTypeF(_, body)))
    }
  }

  private def parseComplexType(node: Node): ResultS[TypeT] = {
    for {
      `type` <- parseComplexType0(node)
      name <- {
        `type`.unfix match {
          case NamedTypeF(name, _) => right(name)
          case _ => leftStr[Ident](s"Can't get the name of type ${`type`}")
        }
      }
    } yield `type`
  }

  private def parseComplexTypes(root: Node): ResultS[List[TypeT]] =
    els("complexType")(root).flatMap(_.traverse(parseComplexType(_)))

  private val XSD_NAMESPACE = "http://www.w3.org/2001/XMLSchema"

  private def findNamespace(xsd: Node): ResultS[Option[String]] =
    right(xml.getPrefix(XSD_NAMESPACE)(xsd))

  private def compilePass(xsd: Node): Result[Module] = {
    val r: ResultS[Module] = for {
      ns <- findNamespace(xsd)
      _ <- updateNs(ns)
      simpleTypesT <- parseSimpleTypes(xsd)
      complexTypesT <- parseComplexTypes(xsd)
      types <- (simpleTypesT ++ complexTypesT).traverse {
        _ match {
          case t @ TypeT(NamedTypeF(f, _)) => right((f, t))
          case t => leftStr[(Ident, TypeT)](s"Got anonymous type $t")
        }
      }
    } yield ModuleF(None, types.toMap)

    r.value.run(ParserState()).value match {
      case (state, result) =>
        result.leftMap(
          e =>
            new Exception(
              s"$e (path: ${state.path.reverse.map(_.label).mkString(".")})",
              e
          )
        )
    }
  }

  private def linkComplexType(m: Module,
                              ctName: Ident,
                              ct: ComplexTypeF[TypeT]): Result[TypeT] = {
    ct.body.body
      .foldLeft(Right(TypeT(NamedTypeF(ctName, ct))): Result[TypeT]) {
        (a, v) =>
          v.unfix match {
            case FieldF(name, TypeT(TypeIdF(ref))) =>
              if ((m.types.contains(ref)))
                a
              else
                Left(
                  new Exception(
                    s"Can't find the type ${ref.name} for the field ${ctName.name}.${name.name}"
                  )
                )
            case _ => a
          }
      }
  }

  private def linkType(m: Module, t: TypeT): Result[TypeT] = {
    t.unfix match {
      case NamedTypeF(name, ct: ComplexTypeF[TypeT]) =>
        linkComplexType(m, name, ct)
      case _ => Right(t)
    }
  }

  private def linkPass(m: Module): Result[Module] = {
    for {
      t <- m.types.traverse(linkType(m, _))
    } yield ModuleF(m.name, t)
  }

  def apply(xsd: Node): Result[Module] = {
    for {
      cm <- compilePass(xsd)
      m <- linkPass(cm)
    } yield m
  }

  def fromStream(reader: java.io.InputStream): Result[Module] = {
    for {
      xsd <- Try(XML.load(reader)).toEither
      module <- this(xsd)
    } yield module
  }
}
