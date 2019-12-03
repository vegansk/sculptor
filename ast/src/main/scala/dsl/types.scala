package sculptor
package ast
package dsl

import org.typelevel.paiges.Doc

trait Builder[+T] {
  def build: T
}

sealed trait NewtypeBuilderState
object NewtypeBuilderState {
  final case class Incomplete() extends NewtypeBuilderState
  final case class Complete() extends NewtypeBuilderState
}

trait NewtypeBuilder[State <: NewtypeBuilderState] {
  def baseType(typeRef: TypeRef): NewtypeBuilder[NewtypeBuilderState.Complete]
  def generic(parameter: GenericDef, rest: GenericDef*): NewtypeBuilder[State]
  def noGeneric: NewtypeBuilder[State]
  def comment(value: String): NewtypeBuilder[State]
  def validator(v: Validator): NewtypeBuilder[State]
  def additionalCode(code: Doc, rest: Doc*): NewtypeBuilder[State]
  def additionalCodeS(code: String, rest: String*): NewtypeBuilder[State]

  def build(implicit ev: State =:= NewtypeBuilderState.Complete): Newtype
}

object NewtypeBuilder {
  implicit class newtypeBuilder(b: NewtypeBuilder[NewtypeBuilderState.Complete])
      extends Builder[Newtype] {
    def build = b.build
  }
}

sealed trait AliasBuilderState
object AliasBuilderState {
  final case class Incomplete() extends AliasBuilderState
  final case class Complete() extends AliasBuilderState
}

trait AliasBuilder[State <: AliasBuilderState] {
  def baseType(typeRef: TypeRef): AliasBuilder[AliasBuilderState.Complete]
  def generic(parameter: GenericDef, rest: GenericDef*): AliasBuilder[State]
  def noGeneric: AliasBuilder[State]
  def comment(value: String): AliasBuilder[State]
  def additionalCode(code: Doc, rest: Doc*): AliasBuilder[State]
  def additionalCodeS(code: String, rest: String*): AliasBuilder[State]

  def build(implicit ev: State =:= AliasBuilderState.Complete): Alias
}

object AliasBuilder {
  implicit class aliasBuilder(b: AliasBuilder[AliasBuilderState.Complete])
      extends Builder[Alias] {
    def build = b.build
  }
}

sealed trait FieldBuilderState
object FieldBuilderState {
  final case class Incomplete() extends FieldBuilderState
  final case class Complete() extends FieldBuilderState
}

trait FieldBuilder[State <: FieldBuilderState] {
  def ofType(typeRef: TypeRef): FieldBuilder[FieldBuilderState.Complete]
  def comment(value: String): FieldBuilder[State]
  def validator(v: Validator): FieldBuilder[State]

  def build(implicit ev: State =:= FieldBuilderState.Complete): FieldDef
}

object FieldBuilder {
  implicit class fieldBuilder(b: FieldBuilder[FieldBuilderState.Complete])
      extends Builder[FieldDef] {
    def build = b.build
  }
}

sealed trait RecordBuilderState
object RecordBuilderState {
  final case class Incomplete() extends RecordBuilderState
  final case class Complete() extends RecordBuilderState
}

trait RecordBuilder[State <: RecordBuilderState] {
  def generic(parameter: GenericDef, rest: GenericDef*): RecordBuilder[State]
  def noGeneric: RecordBuilder[State]
  def field(
    name: String,
    typeRef: TypeRef,
    comment: String = "",
    validator: Option[Validator] = None
  ): RecordBuilder[RecordBuilderState.Complete]
  def fields(
    field: FieldBuilder[FieldBuilderState.Complete],
    rest: FieldBuilder[FieldBuilderState.Complete]*
  ): RecordBuilder[RecordBuilderState.Complete]
  def fieldDefs(field: FieldDef,
                rest: FieldDef*): RecordBuilder[RecordBuilderState.Complete]
  def comment(value: String): RecordBuilder[State]
  def validator(v: Validator): RecordBuilder[State]
  def additionalCode(code: Doc, rest: Doc*): RecordBuilder[State]
  def additionalCodeS(code: String, rest: String*): RecordBuilder[State]

  def build(implicit ev: State =:= RecordBuilderState.Complete): Record
}

object RecordBuilder {
  implicit class recordBuilder(b: RecordBuilder[RecordBuilderState.Complete])
      extends Builder[Record] {
    def build = b.build
  }
}

trait EnumValueBuilder {
  def value(v: String): EnumValueBuilder
  def comment(v: String): EnumValueBuilder

  def build: EnumValue
}

sealed trait EnumBuilderState
object EnumBuilderState {
  final case class Incomplete() extends EnumBuilderState
  final case class Complete() extends EnumBuilderState
}

trait EnumBuilder[State <: EnumBuilderState] {
  def values(value: EnumValueBuilder,
             rest: EnumValueBuilder*): EnumBuilder[EnumBuilderState.Complete]
  def valueDefs(value: EnumValue,
                rest: EnumValue*): EnumBuilder[EnumBuilderState.Complete]
  def comment(value: String): EnumBuilder[State]
  def additionalCode(code: Doc, rest: Doc*): EnumBuilder[State]
  def additionalCodeS(code: String, rest: String*): EnumBuilder[State]

  def build(implicit ev: State =:= EnumBuilderState.Complete): Enum
}

object EnumBuilder {
  implicit class enumBuilder(b: EnumBuilder[EnumBuilderState.Complete])
      extends Builder[Enum] {
    def build = b.build
  }
}

trait ADTConstructorBuilder {
  def generic(parameter: GenericDef, rest: GenericDef*): ADTConstructorBuilder
  def noGeneric: ADTConstructorBuilder
  def field(name: String,
            typeRef: TypeRef,
            comment: String = "",
            validator: Option[Validator] = None): ADTConstructorBuilder
  def fields(
    field: FieldBuilder[FieldBuilderState.Complete],
    rest: FieldBuilder[FieldBuilderState.Complete]*
  ): ADTConstructorBuilder
  def fieldDefs(field: FieldDef, rest: FieldDef*): ADTConstructorBuilder
  def comment(value: String): ADTConstructorBuilder

  def build: ADTConstructor
}

sealed trait ADTBuilderState
object ADTBuilderState {
  final case class Incomplete() extends ADTBuilderState
  final case class Complete() extends ADTBuilderState
}

trait ADTBuilder[State <: ADTBuilderState] {
  def generic(parameter: GenericDef, rest: GenericDef*): ADTBuilder[State]
  def noGeneric: ADTBuilder[State]
  def constructors(
    cons: ADTConstructorBuilder,
    rest: ADTConstructorBuilder*
  ): ADTBuilder[ADTBuilderState.Complete]
  def constructorDefs(
    cons: ADTConstructor,
    rest: ADTConstructor*
  ): ADTBuilder[ADTBuilderState.Complete]
  def comment(value: String): ADTBuilder[State]
  def validator(v: Validator): ADTBuilder[State]
  def additionalCode(code: Doc, rest: Doc*): ADTBuilder[State]
  def additionalCodeS(code: String, rest: String*): ADTBuilder[State]

  def build(implicit ev: State =:= ADTBuilderState.Complete): ADT
}

object ADTBuilder {
  implicit class adtBuilder(b: ADTBuilder[ADTBuilderState.Complete])
      extends Builder[ADT] {
    def build = b.build
  }
}

trait PackageBuilder {
  def types(t: Builder[_ <: TypeDef],
            rest: Builder[_ <: TypeDef]*): PackageBuilder
  def typeDefs(t: TypeDef, rest: TypeDef*): PackageBuilder
  def typ(t: Builder[_ <: TypeDef]): PackageBuilder
  def typeDef(t: TypeDef): PackageBuilder
  def comment(c: String): PackageBuilder
  def additionalCode(code: Doc, rest: Doc*): PackageBuilder
  def additionalCodeS(code: String, rest: String*): PackageBuilder

  def build: Package
}
