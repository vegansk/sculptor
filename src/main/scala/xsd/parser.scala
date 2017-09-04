package sculptor.xsd

import sculptor._
import types._
import scala.util.Try
import scala.xml._
import cats.implicits._

object parser {

  import helpers._

  private object TypeT {
    def apply(v: TypeF[TypeT]): TypeT = Fix(v)

    def unapply(v: TypeT): Option[TypeF[TypeT]] = v.unfix.some
  }

  private type ParsedElement = (Ident, TypeT)

  private def parseTypeName(name: String): ResultS[TypeT] = {
    for {
      ns <- getNs
      pname <- liftE(prefixed(name))
      typ <- {
        pname match {
          case (ns1, "integer") if ns1 === ns => right(TypeT(IntF()))
          case (ns1, "string") if ns1 === ns => right(TypeT(StringF()))
          //TODO: Namespaces!!!
          case (_, name) => right(TypeT(TypeIdF(Ident(name))))
        }
      }
    } yield typ
  }

  private def parseAnonType(node: Node): ResultS[TypeT] = ???

  private def parseElement(node: Node): ResultS[ParsedElement] = {
    withNode(node) {
      for {
        name <- attr("name")(node).map(Ident(_))
        typ <- attrO("type")(node).flatMap(
          t => t.fold(parseAnonType(node))(tname => parseTypeName(tname))
        )
      } yield (name, typ)
    }
  }

  private def parseRestrictedString(name: Ident,
                                    restriction: Node): ResultS[TypeT] = {
    withNode(restriction) {
      for {
        minLength <- optElAttrAsInt("minLength", "value")(restriction)
        maxLength <- optElAttrAsInt("maxLength", "value")(restriction)
        regExp <- els("pattern")(restriction)
          .flatMap(_.traverse(attr("value")(_)))
      } yield
        TypeT(
          RestrictedStringF(
            name = name,
            baseType = TypeT(StringF()),
            minLength = minLength,
            maxLength = maxLength,
            regExp = regExp
          )
        )
    }
  }

  private def parseRestrictedOrdinal(name: Ident,
                                     baseType: TypeT,
                                     restriction: Node): ResultS[TypeT] = {
    withNode(restriction) {
      for {
        minInclusive <- optElAttrAsInt("minInclusive", "value")(restriction)
        maxInclusive <- optElAttrAsInt("maxInclusive", "value")(restriction)
        minExclusive <- optElAttrAsInt("minExclusive", "value")(restriction)
        maxExclusive <- optElAttrAsInt("maxExclusive", "value")(restriction)
        totalDigits <- optElAttrAsInt("totalDigits", "value")(restriction)
        regExp <- els("pattern")(restriction)
          .flatMap(_.traverse(attr("value")(_)))
      } yield
        TypeT(
          RestrictedNumberF(
            name = name,
            baseType = baseType,
            minInclusive = minInclusive,
            maxInclusive = maxInclusive,
            minExclusive = minExclusive,
            maxExclusive = maxExclusive,
            totalDigits = totalDigits,
            regExp = regExp
          )
        )
    }
  }

  private def parseSimpleType(node: Node): ResultS[ParsedElement] = {
    withNode(node) {
      for {
        name <- attr("name")(node).map(Ident(_))
        restriction <- el("restriction")(node)
        base <- attr("base")(restriction).flatMap(parseTypeName(_))
        `type` <- base match {
          case TypeT(StringF()) =>
            parseRestrictedString(name, restriction)
          case TypeT(IntF()) =>
            parseRestrictedOrdinal(name, TypeT(IntF()), restriction)
          case typ => leftStr(s"Unknown base type: $typ")
        }
      } yield (name, `type`)
    }
  }

  private def parseSimpleTypes(root: Node): ResultS[List[ParsedElement]] =
    els("simpleType")(root).flatMap(_.traverse(parseSimpleType(_)))

  private def parseComplexType(node: Node): ResultS[ParsedElement] = {
    withNode(node) {
      for {
        name <- attr("name")(node).map(Ident(_))
        seq <- el("sequence")(node)
        fields <- els("element")(seq).flatMap(_.traverse(parseElement(_)))
      } yield (name, TypeT(RecordF(name, fields)))
    }
  }

  private def parseComplexTypes(root: Node): ResultS[List[ParsedElement]] =
    els("complexType")(root).flatMap(_.traverse(parseComplexType(_)))

  private val XSD_NAMESPACE = "http://www.w3.org/2001/XMLSchema"

  private def findNamespace(xsd: Node): ResultS[Option[String]] =
    right(xml.getPrefix(XSD_NAMESPACE)(xsd))

  private def compilePass(xsd: Node): Result[Module] = {
    val r = for {
      ns <- findNamespace(xsd)
      _ <- updateNs(ns)
      simpleTypes <- parseSimpleTypes(xsd)
      complexTypes <- parseComplexTypes(xsd)
    } yield ModuleF(None, (simpleTypes ++ complexTypes).toMap)

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

  private def linkRecord(m: Module, r: RecordF[TypeT]): Result[TypeT] = {
    r.fields.foldLeft(Right(TypeT(r)): Result[TypeT]) { (a, v) =>
      v._2 match {
        case TypeT(id: TypeIdF[TypeT]) =>
          if ((m.types.contains(id.ref)))
            a
          else
            Left(
              new Exception(
                s"Can't find the type ${id.ref.name} for the field ${r.name.name}.${v._1.name}"
              )
            )
        case _ => a
      }
    }
  }

  private def linkType(m: Module, t: TypeT): Result[TypeT] = {
    t match {
      case TypeT(r: RecordF[TypeT]) => linkRecord(m, r)
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
