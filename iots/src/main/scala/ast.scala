package sculptor
package iots

import shapeless._

object ast {

  type Name = String

  final case class Ident(name: Name, `package`: Option[Ident] = None)

  sealed trait AST

  final case class TypeAlias(typeName: Name,
                             typeRef: Ident,
                             realType: Option[Ident])
      extends AST {
    lazy val constName = typeName + "Type"
  }

  type Type = TypeAlias :+: CNil

  final case class Module(types: List[Type]) extends AST

}
