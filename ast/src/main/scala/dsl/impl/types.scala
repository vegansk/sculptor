package sculptor
package ast
package dsl
package impl

import cats._
import cats.data.NonEmptyList
import cats.implicits._

final case class NewtypeBuilderImpl[State <: NewtypeBuilderState] private (
  value: Newtype
) extends NewtypeBuilder[State] {

  def baseType(typeRef: TypeRef) =
    this.copy(value = value.copy(baseType = typeRef))
  def generic(parameter: GenericDef, rest: GenericDef*) =
    this.copy(value = value.copy(parameters = parameter :: rest.toList))
  def noGeneric =
    this.copy(value = value.copy(parameters = Nil))
  def comment(v: String) =
    this.copy(value = value.copy(comment = v.some))
  def validator(v: Validator) =
    this.copy(value = value.copy(validator = v.some))

  def build(implicit ev: State =:= NewtypeBuilderState.Complete) = this.value
}

object NewtypeBuilderImpl {
  def create(name: String): NewtypeBuilder[NewtypeBuilderState.Incomplete] =
    NewtypeBuilderImpl[NewtypeBuilderState.Incomplete](
      Newtype(Ident(name), Nil, TypeRef.spec(""))
    )
}

final case class AliasBuilderImpl[State <: AliasBuilderState] private (
  value: Alias
) extends AliasBuilder[State] {

  def baseType(typeRef: TypeRef) =
    this.copy(value = value.copy(baseType = typeRef))
  def generic(parameter: GenericDef, rest: GenericDef*) =
    this.copy(value = value.copy(parameters = parameter :: rest.toList))
  def noGeneric =
    this.copy(value = value.copy(parameters = Nil))
  def comment(v: String) =
    this.copy(value = value.copy(comment = v.some))

  def build(implicit ev: State =:= AliasBuilderState.Complete) = this.value
}

object AliasBuilderImpl {
  def create(name: String): AliasBuilder[AliasBuilderState.Incomplete] =
    AliasBuilderImpl[AliasBuilderState.Incomplete](
      Alias(Ident(name), Nil, TypeRef.spec(""))
    )
}

final case class FieldBuilderImpl[State <: FieldBuilderState] private (
  field: FieldDef
) extends FieldBuilder[State] {
  def ofType(typeRef: TypeRef) =
    this.copy(field = field.copy(`type` = typeRef))
  def comment(v: String) =
    this.copy(field = field.copy(comment = v.some))
  def validator(v: Validator) =
    this.copy(field = field.copy(validator = v.some))

  def build(implicit ev: State =:= FieldBuilderState.Complete) = this.field
}

object FieldBuilderImpl {
  def create(name: String): FieldBuilder[FieldBuilderState.Incomplete] =
    FieldBuilderImpl[FieldBuilderState.Incomplete](
      FieldDef(Ident(name), TypeRef.spec(""))
    )
}

final case class RecordBuilderImpl[State <: RecordBuilderState] private (
  value: Record
) extends RecordBuilder[State] {
  def generic(parameter: GenericDef, rest: GenericDef*) =
    this.copy(value = value.copy(parameters = parameter :: rest.toList))
  def noGeneric =
    this.copy(value = value.copy(parameters = Nil))
  def fields(field: FieldBuilder[FieldBuilderState.Complete],
             rest: FieldBuilder[FieldBuilderState.Complete]*) =
    fieldDefs(field.build, rest.map(_.build): _*)
  def field(name: String,
            typeRef: TypeRef,
            comment: String = "",
            validator: Option[Validator] = None) = {
    val fb = ((FieldBuilderImpl
      .create(name)
      .ofType(typeRef)): Id[FieldBuilder[FieldBuilderState.Complete]])
      .map(v => Option(comment).filterNot(_.isEmpty).fold(v)(v.comment _))
      .map(v => validator.fold(v)(v.validator _))
    this.copy(
      value = value.copy(
        fields =
          if (value.fields.head.name.name.isEmpty) NonEmptyList.of(fb.build)
          else value.fields ++ List(fb.build)
      )
    )
  }
  def fieldDefs(field: FieldDef, rest: FieldDef*) =
    this.copy(value = value.copy(fields = NonEmptyList.of(field, rest: _*)))
  def comment(v: String) =
    this.copy(value = value.copy(comment = v.some))
  def validator(v: Validator) =
    this.copy(value = value.copy(validator = v.some))

  def build(implicit ev: State =:= RecordBuilderState.Complete) =
    this.value
}

object RecordBuilderImpl {
  def create(name: String): RecordBuilder[RecordBuilderState.Incomplete] =
    RecordBuilderImpl[RecordBuilderState.Incomplete](
      Record(
        Ident(name),
        Nil,
        NonEmptyList.of(FieldDef(Ident(""), TypeRef.spec("")))
      )
    )
}

final case class EnumValueBuilderImpl private (enumValue: EnumValue)
    extends EnumValueBuilder {
  def value(v: String) =
    this.copy(enumValue = enumValue.copy(value0 = v.some))
  def comment(v: String) =
    this.copy(enumValue = enumValue.copy(comment = v.some))

  def build = this.enumValue
}

object EnumValueBuilderImpl {
  def create(name: String): EnumValueBuilder =
    EnumValueBuilderImpl(EnumValue(Ident(name)))
}

final case class EnumBuilderImpl[State <: EnumBuilderState] private (enum: Enum)
    extends EnumBuilder[State] {
  def values(v: EnumValueBuilder, rest: EnumValueBuilder*) =
    valueDefs(v.build, rest.map(_.build): _*)
  def valueDefs(v: EnumValue, rest: EnumValue*) =
    this.copy(enum = enum.copy(values = NonEmptyList.of(v, rest: _*)))
  def comment(v: String) =
    this.copy(enum = enum.copy(comment = v.some))

  def build(implicit ev: State =:= EnumBuilderState.Complete) =
    this.enum
}

object EnumBuilderImpl {
  def create(name: String): EnumBuilder[EnumBuilderState.Incomplete] =
    EnumBuilderImpl[EnumBuilderState.Incomplete](
      Enum(Ident(name), NonEmptyList.of(EnumValue(Ident(""))))
    )
}

final case class ADTConstructorBuilderImpl private (value: ADTConstructor)
    extends ADTConstructorBuilder {
  def generic(parameter: GenericDef, rest: GenericDef*) =
    this.copy(value = value.copy(parameters = parameter :: rest.toList))
  def noGeneric =
    this.copy(value = value.copy(parameters = Nil))
  def field(name: String,
            typeRef: TypeRef,
            comment: String = "",
            validator: Option[Validator] = None) = {
    val fb = ((FieldBuilderImpl
      .create(name)
      .ofType(typeRef)): Id[FieldBuilder[FieldBuilderState.Complete]])
      .map(v => Option(comment).filterNot(_.isEmpty).fold(v)(v.comment _))
      .map(v => validator.fold(v)(v.validator _))
    this.copy(value = value.copy(fields = value.fields ++ List(fb.build)))
  }
  def fields(field: FieldBuilder[FieldBuilderState.Complete],
             rest: FieldBuilder[FieldBuilderState.Complete]*) =
    fieldDefs(field.build, rest.map(_.build): _*)
  def fieldDefs(field: FieldDef, rest: FieldDef*) =
    this.copy(value = value.copy(fields = field :: rest.toList))
  def comment(v: String) =
    this.copy(value = value.copy(comment = v.some))

  def build = this.value
}

object ADTConstructorBuilderImpl {
  def create(name: String): ADTConstructorBuilder =
    ADTConstructorBuilderImpl(ADTConstructor(Ident(name), Nil, Nil))
}

final case class ADTBuilderImpl[State <: ADTBuilderState] private (value: ADT)
    extends ADTBuilder[State] {
  def generic(parameter: GenericDef, rest: GenericDef*) =
    this.copy(value = value.copy(parameters = parameter :: rest.toList))
  def noGeneric =
    this.copy(value = value.copy(parameters = Nil))
  def constructors(cons: ADTConstructorBuilder, rest: ADTConstructorBuilder*) =
    constructorDefs(cons.build, rest.map(_.build): _*)
  def constructorDefs(cons: ADTConstructor, rest: ADTConstructor*) =
    this.copy(
      value = value.copy(constructors = NonEmptyList.of(cons, rest: _*))
    )
  def comment(v: String) =
    this.copy(value = value.copy(comment = v.some))
  def validator(v: Validator) =
    this.copy(value = value.copy(validator = v.some))

  def build(implicit ev: State =:= ADTBuilderState.Complete) =
    this.value
}

object ADTBuilderImpl {
  def create(name: String): ADTBuilder[ADTBuilderState.Incomplete] =
    ADTBuilderImpl[ADTBuilderState.Incomplete](
      ADT(
        Ident(name),
        Nil,
        NonEmptyList.of(ADTConstructor(Ident(""), Nil, Nil))
      )
    )
}

final case class PackageBuilderImpl private (value: Package)
    extends PackageBuilder {
  def types(t: Builder[_ <: TypeDef], rest: Builder[_ <: TypeDef]*) =
    typeDefs(t.build, rest.map(_.build): _*)
  def typeDefs(t: TypeDef, rest: TypeDef*) =
    this.copy(value = value.copy(types = t :: rest.toList))
  def typ(t: Builder[_ <: TypeDef]) =
    this.typeDef(t.build)
  def typeDef(t: TypeDef) =
    this.copy(value = value.copy(types = this.value.types ++ List(t)))
  def comment(c: String) =
    this.copy(value = value.copy(comment = c.some))

  def build = this.value
}

object PackageBuilderImpl {
  def create(name: String): PackageBuilder =
    PackageBuilderImpl(Package(FQName.of(name), Nil, None))
}
