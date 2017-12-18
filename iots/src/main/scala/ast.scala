package sculptor
package iots

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

  sealed trait TypeRef
  object TypeRef {
    final case class std(name: Ident) extends TypeRef
    final case class defined(name: Ident, constName: Ident) extends TypeRef
    final case class external(name: QName, constName: QName) extends TypeRef

    def definedFrom(name: String, constName: String): defined =
      defined(Ident(name), Ident(constName))
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
                             `type`: TypeRef,
                             constraint: FieldConstraint,
                             array: Boolean)

  sealed trait TypeDecl {
    def `type`: TypeRef.defined
  }

  final case class ComplexTypeDecl(`type`: TypeRef.defined,
                                   baseType: Option[TypeRef],
                                   exported: Boolean,
                                   fields: NEL[FieldDecl])
      extends TypeDecl

  final case class EnumMemberDecl(name: Ident, value: String)

  final case class EnumDecl(`type`: TypeRef.defined,
                            exported: Boolean,
                            members: NEL[EnumMemberDecl])
      extends TypeDecl

  final case class NewtypeDecl(`type`: TypeRef.defined,
                               baseType: TypeRef,
                               exported: Boolean)
      extends TypeDecl

  final case class TypesDecl(value: NEL[TypeDecl])

  type Path = String

  final case class ImportDecl(name: Ident, path: Path)

  final case class ImportsDecl(value: List[ImportDecl])

  final case class ModuleDecl(imports: Option[ImportsDecl],
                              types: Option[TypesDecl])

}
