package sculptor
package scalagen
package xsd

import scala.util.Try
import cats.implicits._

import sculptor.xsd.{ast => x}

object Fold {
  type SimpleTypeEnumHandler[A] =
    ( /*name: */ String, /*values: */ List[x.Enumeration[SrcF]]) => A
  type SimpleTypeNewtypeHandler[A] =
    ( /*name: */ String, /*base: */ x.QName,
     /*ann: */ Option[x.Annotation[SrcF]]) => A
  type SimpleTypeHandler[A] = x.SimpleType[SrcF] => A

  type BodyFieldHandler[A] =
    ( /*name: */ String, /*`type`: */ Option[x.QName],
     /*minOccurs: */ Option[Int],
     /*maxOccurs: */ Option[Int], /*nullable: */ Boolean,
     /*ann: */ Option[x.Annotation[SrcF]]) => A
  type BodySequenceHandler[A] =
    ( /*seq: */ x.Sequence[SrcF], /*minOccurs: */ Option[Int],
     /*maxOccurs: */ Option[Int]) => A
  type BodyChoiceHandler[A] =
    ( /*seq: */ x.Choice[SrcF], /*minOccurs: */ Option[Int],
     /*maxOccurs: */ Option[Int]) => A
  type BodyAnonComplexTypeHandler[A] =
    ( /*name: */ String, /*ct: */ x.ComplexType[SrcF],
     /*minOccurs: */ Option[Int],
     /*maxOccurs: */ Option[Int], /*nullable: */ Boolean,
     /*ann: */ Option[x.Annotation[SrcF]]) => A
  type BodyAnonSimpleTypeHandler[A] =
    ( /*name: */ String, /*st: */ x.SimpleType[SrcF],
     /*minOccurs: */ Option[Int],
     /*maxOccurs: */ Option[Int], /*nullable: */ Boolean,
     /*ann: */ Option[x.Annotation[SrcF]]) => A
  type BodyHandler[A] = x.Body[SrcF] => A

  type ComplexTypeSequenceHandler[A] =
    ( /*name: */ String, /*baseName: */ Option[x.QName],
     /*seq: */ x.Sequence[SrcF], /*attrs: */ List[x.Attribute[SrcF]]) => A
  type ComplexTypeChoiceHandler[A] =
    ( /*name: */ String, /*seq: */ x.Choice[SrcF],
     /*attrs: */ List[x.Attribute[SrcF]]) => A
  type ComplexTypeAnyHandler[A] =
    ( /*name: String*/ String, /*any: */ x.Any[SrcF]) => A
  type ComplexTypeAliasHandler[A] =
    ( /*name: */ String, /*base: */ x.QName,
     /**/ Option[x.Annotation[SrcF]]) => A
  type ComplexTypeHandler[A] = x.ComplexType[SrcF] => A

  type ElementComplexTypeHandler[A] =
    ( /*name: */ String, /*ct: */ x.ComplexType[SrcF]) => A
  type ElementHandler[A] = x.Element[SrcF] => A
}

final class Fold(config: Config) {

  import Fold._

  def simpleType[A](onEnum: SimpleTypeEnumHandler[A],
                    onNewtype: SimpleTypeNewtypeHandler[A],
                    default: SimpleTypeHandler[A])(t: x.SimpleType[SrcF]): A =
    t match {
      case x.SimpleType(
          _,
          Some(name),
          _,
          Some(
            x.SimpleTypeRestriction(
              x.QName("string", config.xsdNs),
              None,
              _,
              _,
              values @ (_ :: _)
            )
          )
          ) =>
        onEnum(name, values)
      case x.SimpleType(
          ann,
          Some(name),
          _,
          Some(x.SimpleTypeRestriction(base, _, _, _, Nil))
          ) =>
        onNewtype(name, base, ann)
      case _ => default(t)
    }

  object occurs {
    def unapply(v: Option[String]): Option[Option[Int]] =
      v match {
        case None => 1.some.some
        case Some("unbounded") => none[Int].some
        case Some(str) => Try(str.toInt).toOption.map(Some(_))
      }
  }

  object isNullable {
    def unapply(v: Option[String]): Option[Boolean] = v match {
      case Some("true") => Some(true)
      case _ => Some(false)
    }
  }

  def body[A](onField: BodyFieldHandler[A],
              onSeq: BodySequenceHandler[A],
              onChoice: BodyChoiceHandler[A],
              onAnonComplexType: BodyAnonComplexTypeHandler[A],
              onAnonSimpleType: BodyAnonSimpleTypeHandler[A],
              default: BodyHandler[A])(b: x.Body[SrcF]): A =
    x.Body.fold(
      (el: x.Element[SrcF]) => {
        el match {
          case x.Element(
              ann,
              Some(name),
              None,
              None,
              typ,
              occurs(minOccurs),
              occurs(maxOccurs),
              isNullable(nullable)
              ) =>
            onField(
              name,
              typ.map(x.QName.fromString _),
              minOccurs,
              maxOccurs,
              nullable,
              ann
            )
          case x.Element(
              ann,
              Some(name),
              Some(ct),
              None,
              None,
              occurs(minOccurs),
              occurs(maxOccurs),
              isNullable(nullable)
              ) =>
            onAnonComplexType(name, ct, minOccurs, maxOccurs, nullable, ann)
          case x.Element(
              ann,
              Some(name),
              None,
              Some(st),
              None,
              occurs(minOccurs),
              occurs(maxOccurs),
              isNullable(nullable)
              ) =>
            onAnonSimpleType(name, st, minOccurs, maxOccurs, nullable, ann)
          case _ => default(b)
        }
      },
      (seq: x.Sequence[SrcF]) =>
        seq match {
          case x.Sequence(_, _, occurs(minOccurs), occurs(maxOccurs)) =>
            onSeq(seq, minOccurs, maxOccurs)
          case _ => default(b)
      },
      (ch: x.Choice[SrcF]) =>
        ch match {
          case x.Choice(_, _, occurs(minOccurs), occurs(maxOccurs)) =>
            onChoice(ch, minOccurs, maxOccurs)
          case _ => default(b)
      },
      (any: x.Any[SrcF]) => default(b)
    )(b)

  def complexType[A](
    onSequence: ComplexTypeSequenceHandler[A],
    onChoice: ComplexTypeChoiceHandler[A],
    onAny: ComplexTypeAnyHandler[A],
    onAlias: ComplexTypeAliasHandler[A],
    default: ComplexTypeHandler[A]
  )(t: x.ComplexType[SrcF]): A = t match {
    case x.ComplexType(
        _,
        Some(name),
        None,
        Some(seq),
        None,
        attrs,
        _,
        _,
        _,
        _
        ) =>
      seq match {
        case x.Sequence(_, List(x.Body.any(any)), _, _) => onAny(name, any)
        case _ => onSequence(name, None, seq, attrs)
      }
    case x.ComplexType(
        _,
        Some(name),
        Some(
          x.ComplexContent(
            _,
            Some(x.ComplexContentExtension(_, baseName, seqO))
          )
        ),
        None,
        None,
        attrs,
        _,
        _,
        _,
        _
        ) =>
      seqO match {
        case Some(seq) => onSequence(name, baseName.some, seq, attrs)
        case _ => onAlias(name, baseName, t.annotation)
      }
    case x.ComplexType(
        _,
        Some(name),
        None,
        None,
        Some(ch),
        attrs,
        _,
        _,
        _,
        _
        ) =>
      onChoice(name, ch, attrs)
    case _ => default(t)
  }

  def element[A](onComplexType: ElementComplexTypeHandler[A],
                 default: ElementHandler[A])(e: x.Element[SrcF]): A = e match {
    case x.Element(_, Some(name), Some(ct), _, _, _, _, _) =>
      onComplexType(name, ct)
    case _ => default(e)
  }
}
