package sculptor
package ast

import cats.data._
import cats.implicits._

/** Identifier representation */
final case class Ident(name: String) extends AnyVal

/** Full qualified name */
final case class FQName(name: Ident, prefix: List[Ident] = Nil) {
  def mkString(sep: String): String =
    (prefix ++ List(name)).map(_.name).mkString(sep)
}

object FQName {
  def of(name: String): FQName = {
    name.split("\\.").toList.toNel.fold(FQName(Ident(name))) { l =>
      val names = l.map(Ident(_))
      FQName(names.last, names.init)
    }
  }
}

/** Types references ADT, used in type definitions */
sealed trait TypeRef {
  def asString: String
}

object TypeRef {

  /** Specialized type reference */
  final case class Specialized(name: FQName, parameters: List[TypeRef] = Nil)
      extends TypeRef {
    def asString = name.mkString(".")
  }

  /** Generic type reference */
  final case class Generic(name: Ident) extends TypeRef {
    def asString = name.name
  }

  def spec(name: String, parameters: TypeRef*): Specialized =
    Specialized(FQName.of(name), parameters.toList)

  def gen(name: String): Generic = Generic(Ident(name))

  def cata[A](specialized: Specialized => A,
              generic: Generic => A)(t: TypeRef): A = t match {
    case s: Specialized => specialized(s)
    case g: Generic => generic(g)
  }
}

/** Generic parameter definition */
final case class GenericDef(`type`: TypeRef.Generic,
                            `extends`: List[TypeRef] = Nil)

object GenericDef {
  implicit def genericDefFromGeneric(g: TypeRef.Generic): GenericDef =
    GenericDef(g)

  def of(name: String, `extends`: TypeRef*): GenericDef =
    GenericDef(TypeRef.gen(name), `extends`.toList)
}

/** Types definitions ADT */
sealed trait TypeDef {
  def name: Ident
  def comment: Option[String]
  def ref: TypeRef
  def isNewtype: Boolean = false
  def isAlias: Boolean = false
  def isRecord: Boolean = false
  def isEnum: Boolean = false
  def isADT: Boolean = false
}

object TypeDef {

  def cata[A](newtype: Newtype => A,
              alias: Alias => A,
              record: Record => A,
              enum: Enum => A,
              adt: ADT => A): TypeDef => A = {
    case t: Newtype => newtype(t)
    case t: Alias => alias(t)
    case t: Record => record(t)
    case t: Enum => enum(t)
    case t: ADT => adt(t)
  }

}

/** Validator */
sealed trait Validator

final case class ValidationFunction(name: String) extends Validator

/** Newtype definition */
final case class Newtype(name: Ident,
                         parameters: List[GenericDef],
                         baseType: TypeRef,
                         comment: Option[String] = None,
                         validator: Option[Validator] = None)
    extends TypeDef {
  lazy val ref: TypeRef =
    TypeRef.Specialized(FQName(name), parameters.map(_.`type`))

  override def isNewtype = true
}

/** Alias */
final case class Alias(name: Ident,
                       parameters: List[GenericDef],
                       baseType: TypeRef,
                       comment: Option[String] = None)
    extends TypeDef {
  lazy val ref: TypeRef =
    TypeRef.Specialized(FQName(name), parameters.map(_.`type`))

  override def isAlias = true
}

/** Field definition */
final case class FieldDef(name: Ident,
                          `type`: TypeRef,
                          comment: Option[String] = None,
                          validator: Option[Validator] = None)

/** Record */
final case class Record(name: Ident,
                        parameters: List[GenericDef],
                        fields: NonEmptyList[FieldDef],
                        comment: Option[String] = None,
                        validator: Option[Validator] = None)
    extends TypeDef {
  lazy val ref: TypeRef =
    TypeRef.Specialized(FQName(name), parameters.map(_.`type`))

  override def isRecord = true
}

/** Enumeration value */
final case class EnumValue(name: Ident,
                           private[ast] val value0: Option[String] = None,
                           comment: Option[String] = None) {
  def value: String = value0.getOrElse(name.name)
}

/** Enumeration */
final case class Enum(name: Ident,
                      values: NonEmptyList[EnumValue],
                      comment: Option[String] = None)
    extends TypeDef {
  lazy val ref: TypeRef = TypeRef.Specialized(FQName(name))

  override def isEnum = true
}

/** ADT constructor definition */
final case class ADTConstructor(name: Ident,
                                parameters: List[GenericDef],
                                fields: List[FieldDef],
                                comment: Option[String] = None,
                                tag: Option[String] = None) {
  def ref: TypeRef =
    TypeRef.Specialized(FQName.of(name.name), parameters.map(_.`type`))
}

/** ADT definition */
final case class ADT(name: Ident,
                     parameters: List[GenericDef],
                     constructors: NonEmptyList[ADTConstructor],
                     comment: Option[String] = None,
                     validator: Option[Validator] = None)
    extends TypeDef {
  lazy val ref: TypeRef =
    TypeRef.Specialized(FQName(name), parameters.map(_.`type`))

  override def isADT = true
}

/** Types package */
final case class Package(name: FQName,
                         types: List[TypeDef],
                         comment: Option[String])
