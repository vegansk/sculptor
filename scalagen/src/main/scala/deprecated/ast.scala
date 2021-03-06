package sculptor.scalagen.deprecated

import cats._
import cats.implicits._
import cats.data.{NonEmptyList => NEL}

object ast {

  final case class Ident(value: String)

  final case class QName(path: NEL[Ident])
  object QName {
    def of(x: Ident, xs: Ident*): QName =
      QName(new NEL(x, xs.toList))
    def fromString(s: String): QName =
      NEL
        .fromList(s.split("\\.").toList.map(Ident(_)))
        .map(QName(_))
        .getOrElse(QName.of(Ident(s)))
  }

  sealed trait TypeRef {
    val isDefined: Boolean = false
  }
  object TypeRef {
    final case class std(name: Ident) extends TypeRef
    final case class defined(name: Ident) extends TypeRef {
      override val isDefined = true
    }
    final case class external(name: QName) extends TypeRef

    def definedFrom(name: String): defined = defined(Ident(name))
  }

  sealed trait FieldConstraint
  object FieldConstraint {
    object Optional extends FieldConstraint
    object Nullable extends FieldConstraint
    object OptionalNullable extends FieldConstraint
    object Required extends FieldConstraint

    implicit val FieldConstraintEq: Eq[FieldConstraint] =
      Eq.fromUniversalEquals[FieldConstraint]
  }

  final case class FieldDecl(name: Ident,
                             xmlName: String,
                             `type`: TypeRef,
                             constraint: FieldConstraint,
                             attribute: Boolean,
                             array: Boolean,
                             comment: Option[Comment])

  type Comment = String

  sealed trait TypeDecl {
    def `type`: TypeRef.defined
    def comment: Option[Comment]
  }

  final case class ComplexTypeDecl(`type`: TypeRef.defined,
                                   baseType: Option[TypeRef],
                                   fields: NEL[FieldDecl],
                                   comment: Option[Comment])
      extends TypeDecl

  final case class SimpleTypeExtensionDecl(`type`: TypeRef.defined,
                                           baseType: TypeRef,
                                           fields: NEL[FieldDecl],
                                           comment: Option[Comment])
      extends TypeDecl

  final case class EnumMemberDecl(name: Ident,
                                  value: String,
                                  comment: Option[Comment])

  final case class EnumDecl(`type`: TypeRef.defined,
                            members: NEL[EnumMemberDecl],
                            comment: Option[Comment])
      extends TypeDecl

  final case class NewtypeDecl(`type`: TypeRef.defined,
                               baseType: TypeRef,
                               comment: Option[Comment])
      extends TypeDecl

  final case class TypesDecl(value: NEL[TypeDecl])

  type Path = String

  final case class ImportDecl(path: Path)

  final case class ImportsDecl(value: List[ImportDecl])

  final case class ModuleDecl(imports: Option[ImportsDecl],
                              types: Option[TypesDecl])

}
