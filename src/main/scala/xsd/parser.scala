package sculptor.xsd

import sculptor._
import scala.util.Try
import scala.xml._
import cats._, cats.implicits._, cats.data._

object parser {

  import helpers._

  private def parseTypeName(name: String): ResultS[CType] = {
    for {
      ns <- getNs
      p <- liftE(prefixed(name))
      `type` <- p match {
        case (ns1, "integer") if ns1 == ns => right(LinkedType(types.Int))
        case (ns1, "string") if ns1 == ns => right(LinkedType(types.Str))
        case (ns1, name) => right(UnlinkedType(name))
      }
    } yield `type`
  }

  private def parseAnonType(node: Node): ResultS[CType] = ???

  private def parseElement(node: Node): ResultS[ParsedElement] = {
    withNode(node) {
      for {
        name <- attr("name")(node)
        typ <- attrO("type")(node).flatMap(t => t.fold(parseAnonType(node))(tname => parseTypeName(tname)))
      } yield (name, typ)
    }
  }

  private def parseRestrictedString(name: Option[String], restriction: Node): ResultS[types.RStr] = {
    withNode(restriction) {
      for {
        minLength <- optElAttrAsInt("minLength", "value")(restriction)
        maxLength <- optElAttrAsInt("maxLength", "value")(restriction)
        regExp <- els("pattern")(restriction).flatMap(_.traverse(attr("value")(_)))
      } yield types.RStr(name = name, minLength = minLength, maxLength = maxLength, regExp = regExp)
    }
  }

  private def parseRestrictedOrdinal(name: Option[String], `type`: types.Ordinal, restriction: Node): ResultS[types.ROrdinal] = {
    withNode(restriction) {
      for {
        minInclusive <- optElAttrAsInt("minInclusive", "value")(restriction)
        maxInclusive <- optElAttrAsInt("maxInclusive", "value")(restriction)
        minExclusive <- optElAttrAsInt("minExclusive", "value")(restriction)
        maxExclusive <- optElAttrAsInt("maxExclusive", "value")(restriction)
        totalDigits <- optElAttrAsInt("totalDigits", "value")(restriction)
        regExp <- els("pattern")(restriction).flatMap(_.traverse(attr("value")(_)))
      } yield types.ROrdinal(
        name = name,
        `type` = `type`,
        minInclusive = minInclusive,
        maxInclusive = maxInclusive,
        minExclusive = minExclusive,
        maxExclusive = maxExclusive,
        totalDigits = totalDigits,
        regExp = regExp
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
          case LinkedType(types.Str) => parseRestrictedString(name.some, restriction).map(LinkedType)
          case LinkedType(types.Int) => parseRestrictedOrdinal(name.some, types.Int, restriction).map(LinkedType)
          case typ => left(new Exception(s"Unknown base type: $typ"))
        }
      } yield typ
    }
  }

  private def parseSimpleTypes(root: Node): ResultS[List[CType]] =
    els("simpleType")(root).flatMap(_.traverse(parseSimpleType(_)))

  private def parseComplexType(node: Node): ResultS[CRecord] = {
    withNode(node) {
      for {
        name <- attr("name")(node)
        seq <- el("sequence")(node)
        fields <- els("element")(seq).flatMap(_.traverse(parseElement(_)))
      } yield CRecord(Some(name), Map(fields:_*))
    }
  }

  private def parseComplexTypes(root: Node): ResultS[List[CRecord]] =
    els("complexType")(root).flatMap(_.traverse(parseComplexType(_)))

  private val XSD_NAMESPACE = "http://www.w3.org/2001/XMLSchema"

  private def findNamespace(xsd: Node): ResultS[Option[String]] =
    right(xml.getPrefix(XSD_NAMESPACE)(xsd))

  private[sculptor] def compilePass(xsd: Node): Result[CModule] = {
    val r = for {
      ns <- findNamespace(xsd)
      _ <- updateNs(ns)
      simpleTypes <- parseSimpleTypes(xsd)
      complexTypes <- parseComplexTypes(xsd)
    } yield CModule(None, simpleTypes ++ complexTypes)

    r.value.run(ParserState()).value match {
      case (state, result) => result.leftMap(e => new Exception(s"$e (path: ${state.path.reverse.map(_.label).mkString(".")})", e))
    }
  }

  private def linkType(t: CType): Result[types.Type] = t match {
    case LinkedType(t) => Right(t)
    case UnlinkedType(name) => Left(new Throwable(s"Can't find type $name"))
    case CRecord(n, t) => t.traverse(linkType(_)).map(types.Record(n, _))
  }

  private def linkPass(m: CModule): Result[types.Module] = {
    for {
      t <- m.types.traverse(linkType(_))
    } yield types.Module(m.name, t)
  }

  def apply(xsd: Node): Result[types.Module] = {
    for {
      cm <- compilePass(xsd)
      m <- linkPass(cm)
    } yield m
  }

  def apply(reader: java.io.InputStream): Result[types.Module] = {
    for {
      xsd <- Try(XML.load(reader)).toEither
      module <- this(xsd)
    } yield module
  }
}
