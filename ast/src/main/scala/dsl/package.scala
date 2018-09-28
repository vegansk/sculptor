package sculptor
package ast

package object dsl {

  def newtype(name: String): NewtypeBuilder[NewtypeBuilderState.Incomplete] =
    impl.NewtypeBuilderImpl.create(name)

  def alias(name: String): AliasBuilder[AliasBuilderState.Incomplete] =
    impl.AliasBuilderImpl.create(name)

  def field(name: String): FieldBuilder[FieldBuilderState.Incomplete] =
    impl.FieldBuilderImpl.create(name)

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def field(name: String,
            `type`: TypeRef): FieldBuilder[FieldBuilderState.Complete] =
    impl.FieldBuilderImpl.create(name).ofType(`type`)

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

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  implicit class DslTypesBuilder(name: String) {
    def gen: TypeRef.Generic = TypeRef.gen(name)
    def genExt(`extends`: TypeRef, rest: TypeRef*): GenericDef =
      GenericDef.of(name, (`extends` :: rest.toList): _*)
    def spec: TypeRef.Specialized = TypeRef.spec(name)
    def spec(p: TypeRef, rest: TypeRef*): TypeRef.Specialized =
      TypeRef.spec(name, (p :: rest.toList): _*)
    def en: EnumValueBuilder = enumValue(name)
  }
}
