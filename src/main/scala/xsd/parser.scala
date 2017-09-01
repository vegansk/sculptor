package sculptor.xsd

import sculptor._
import types._
import scala.util.Try
import scala.xml._
import cats.implicits._

object parser {

  import helpers._

  private final case class Ident(value: String)

  private type CTypeF[A] = Either[Ident, TypeF[A]]
  private type CType = Fix[CTypeF]

  private object CType {
    def apply(v: TypeF[CType]): CType = Fix(Right(v): CTypeF[CType])

    def unapply(v: CType): Option[CTypeF[CType]] = v.unfix.some

    def toTypeT(v: CType): Result[TypeT] =
      v.unfix
        .flatMap(_ match {
          case IntF() => Right(TypeT(IntF()))
          case StringF() => Right(TypeT(StringF()))
          case RestrictedStringF(n, bt, minL, maxL, re) => {
            toTypeT(bt)
              .map(bt => TypeT(RestrictedStringF(n, bt, minL, maxL, re)))
          }
          case RestrictedNumberF(n, bt, minI, maxI, minE, maxE, td, re) => {
            toTypeT(bt).map(
              bt =>
                TypeT(RestrictedNumberF(n, bt, minI, maxI, minE, maxE, td, re))
            )
          }
          case RecordF(n, f) => {
            f.traverse(f => toTypeT(f._2).map(t => (f._1, t)))
              .map(f => TypeT(RecordF(n, f)))
          }
        })
        .leftMap(name => new Exception(s"Found unknown identifier ${name}"))
  }

  private object CIdent {
    def apply(v: String): CType = Fix(Left(Ident(v)): CTypeF[CType])
  }

  private type ParsedElement = (String, CType)

  private def parseTypeName(name: String): ResultS[CType] = {
    for {
      ns <- getNs
      p <- liftE(prefixed(name))
      `type` <- p match {
        case (ns1, "integer") if ns1 === ns => right(CType(IntF()))
        case (ns1, "string") if ns1 === ns => right(CType(StringF()))
        case (_, name) => right(CIdent(name))
      }
    } yield `type`
  }

  private def parseAnonType(node: Node): ResultS[CType] = ???

  private def parseElement(node: Node): ResultS[ParsedElement] = {
    withNode(node) {
      for {
        name <- attr("name")(node)
        typ <- attrO("type")(node).flatMap(
          t => t.fold(parseAnonType(node))(tname => parseTypeName(tname))
        )
      } yield (name, typ)
    }
  }

  private def parseRestrictedString(name: Option[String],
                                    restriction: Node): ResultS[CType] = {
    withNode(restriction) {
      for {
        minLength <- optElAttrAsInt("minLength", "value")(restriction)
        maxLength <- optElAttrAsInt("maxLength", "value")(restriction)
        regExp <- els("pattern")(restriction)
          .flatMap(_.traverse(attr("value")(_)))
      } yield
        CType(
          RestrictedStringF(
            name = name,
            baseType = CType(StringF()),
            minLength = minLength,
            maxLength = maxLength,
            regExp = regExp
          )
        )
    }
  }

  private def parseRestrictedOrdinal(name: Option[String],
                                     baseType: CType,
                                     restriction: Node): ResultS[CType] = {
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
        CType(
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

  private def parseSimpleType(node: Node): ResultS[CType] = {
    withNode(node) {
      for {
        name <- attr("name")(node)
        restriction <- el("restriction")(node)
        base <- attr("base")(restriction).flatMap(parseTypeName(_))
        typ <- base match {
          case CType(Right(StringF())) =>
            parseRestrictedString(name.some, restriction)
          case CType(Right(IntF())) =>
            parseRestrictedOrdinal(name.some, CType(IntF()), restriction)
          case typ => left(new Exception(s"Unknown base type: $typ"))
        }
      } yield typ
    }
  }

  private def parseSimpleTypes(root: Node): ResultS[List[CType]] =
    els("simpleType")(root).flatMap(_.traverse(parseSimpleType(_)))

  private def parseComplexType(node: Node): ResultS[CType] = {
    withNode(node) {
      for {
        name <- attr("name")(node)
        seq <- el("sequence")(node)
        fields <- els("element")(seq).flatMap(_.traverse(parseElement(_)))
      } yield CType(RecordF(Some(name), fields))
    }
  }

  private def parseComplexTypes(root: Node): ResultS[List[CType]] =
    els("complexType")(root).flatMap(_.traverse(parseComplexType(_)))

  private val XSD_NAMESPACE = "http://www.w3.org/2001/XMLSchema"

  private def findNamespace(xsd: Node): ResultS[Option[String]] =
    right(xml.getPrefix(XSD_NAMESPACE)(xsd))

  private def compilePass(xsd: Node): Result[ModuleF[CType]] = {
    val r = for {
      ns <- findNamespace(xsd)
      _ <- updateNs(ns)
      simpleTypes <- parseSimpleTypes(xsd)
      complexTypes <- parseComplexTypes(xsd)
    } yield ModuleF(None, simpleTypes ++ complexTypes)

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

  private def linkPass(m: ModuleF[CType]): Result[types.Module] = {
    for {
      t <- m.types.traverse(CType.toTypeT(_))
    } yield ModuleF(m.name, t)
  }

  def apply(xsd: Node): Result[types.Module] = {
    for {
      cm <- compilePass(xsd)
      m <- linkPass(cm)
    } yield m
  }

  def fromStream(reader: java.io.InputStream): Result[types.Module] = {
    for {
      xsd <- Try(XML.load(reader)).toEither
      module <- this(xsd)
    } yield module
  }
}
