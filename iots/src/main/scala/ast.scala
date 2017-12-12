package sculptor
package iots

import cats._
import cats.data.{NonEmptyList => NEL}

object ast {

  final case class Ident(value: String)

  final case class QName(path: NEL[Ident])
  object QName {
    def of(x: Ident, xs: Ident*): QName =
      QName(new NEL(x, xs.toList))
  }

  sealed trait TypeName
  object TypeName {
    final case class std(name: Ident) extends TypeName
    final case class custom(name: QName) extends TypeName
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

  final case class TypeRef(`type`: TypeName, constName: QName)

  final case class FieldDecl(name: Ident,
                             `type`: TypeRef,
                             constraint: FieldConstraint,
                             array: Boolean)

  sealed trait TypeDecl

  final case class ComplexTypeDecl(name: Ident,
                                   constName: Ident,
                                   baseType: Option[TypeRef],
                                   exported: Boolean,
                                   fields: NEL[FieldDecl])
      extends TypeDecl

  final case class EnumMemberDecl(name: Ident, value: String)

  final case class EnumDecl(name: Ident,
                            constName: Ident,
                            exported: Boolean,
                            members: NEL[EnumMemberDecl])
      extends TypeDecl

  final case class NewtypeDecl(name: Ident,
                               constName: Ident,
                               baseType: TypeRef,
                               exported: Boolean)
      extends TypeDecl

  final case class TypesDecl(value: NEL[TypeDecl])

  type Path = String

  final case class ImportDecl(name: Ident, path: Path)

  final case class ImportsDecl(value: NEL[ImportDecl])

  final case class ModuleDecl(imports: Option[ImportsDecl],
                              types: Option[TypesDecl])

}
