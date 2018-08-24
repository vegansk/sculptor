package sculptor
package ast

package object dsl {

  def newtype(name: String): NewtypeBuilder[NewtypeBuilderState.Incomplete] =
    impl.NewtypeBuilderImpl.create(name)

  def alias(name: String): AliasBuilder[AliasBuilderState.Incomplete] =
    impl.AliasBuilderImpl.create(name)

  def field(name: String): FieldBuilder[FieldBuilderState.Incomplete] =
    impl.FieldBuilderImpl.create(name)

  def record(name: String): RecordBuilder[RecordBuilderState.Incomplete] =
    impl.RecordBuilderImpl.create(name)

  def enumValue(name: String): EnumValueBuilder =
    impl.EnumValueBuilderImpl.create(name)

  def enum(name: String): EnumBuilder[EnumBuilderState.Incomplete] =
    impl.EnumBuilderImpl.create(name)

  def cons(name: String): ADTConstructorBuilder =
    impl.ADTConstructorBuilderImpl.create(name)

  def adt(name: String): ADTBuilder[ADTBuilderState.Incomplete] =
    impl.ADTBuilderImpl.create(name)

  def pkg(name: String): PackageBuilder =
    impl.PackageBuilderImpl.create(name)
}
