package sculptor

object types {

  type Number = BigDecimal

  final case class Ident(name: String)

  sealed trait TypeF[A]

  final case class AnyF[A]() extends TypeF[A]

  final case class StringF[A]() extends TypeF[A]

  final case class IntegerF[A]() extends TypeF[A]

  final case class NonNegativeIntegerF[A]() extends TypeF[A]

  final case class ByteF[A]() extends TypeF[A]

  final case class IntF[A]() extends TypeF[A]

  final case class LongF[A]() extends TypeF[A]

  final case class NegativeIntegerF[A]() extends TypeF[A]

  final case class NonPositiveIntegerF[A]() extends TypeF[A]

  final case class PositiveIntegerF[A]() extends TypeF[A]

  final case class ShortF[A]() extends TypeF[A]

  final case class UnsignedLongF[A]() extends TypeF[A]

  final case class UnsignedIntF[A]() extends TypeF[A]

  final case class UnsignedShortF[A]() extends TypeF[A]

  final case class UnsignedByteF[A]() extends TypeF[A]

  final case class DecimalF[A]() extends TypeF[A]

  final case class DateF[A]() extends TypeF[A]

  final case class DateTimeF[A]() extends TypeF[A]

  final case class TimeF[A]() extends TypeF[A]

  final case class DurationF[A]() extends TypeF[A]

  final case class BooleanF[A]() extends TypeF[A]

  final case class DoubleF[A]() extends TypeF[A]

  final case class FloatF[A]() extends TypeF[A]

  final case class TypeIdF[A](ref: Ident) extends TypeF[A]

  final case class NamedTypeF[A](name: Ident, `type`: TypeF[A])
      extends TypeF[A]

  final case class RestrictedStringF[A](baseType: A,
                                        minLength: Option[Number] = None,
                                        maxLength: Option[Number] = None,
                                        regExp: List[String] = Nil)
      extends TypeF[A]

  final case class RestrictedNumberF[A](baseType: A,
                                        minInclusive: Option[Number] = None,
                                        maxInclusive: Option[Number] = None,
                                        minExclusive: Option[Number] = None,
                                        maxExclusive: Option[Number] = None,
                                        totalDigits: Option[Number] = None,
                                        regExp: List[String] = Nil)
      extends TypeF[A]

  final case class FieldF[A](name: Ident, `type`: A) extends TypeF[A]

  sealed trait SeqLike[A] { def body: List[A] }

  final case class Sequence[A](val body: List[A]) extends SeqLike[A]

  final case class Choice[A](val body: List[A]) extends SeqLike[A]

  final case class All[A](val body: List[A]) extends SeqLike[A]

  final case class ComplexTypeF[A](body: SeqLike[A]) extends TypeF[A]

  final case class ModuleF[A](name: Option[String], types: Map[Ident, A])

  type TypeT = Fix[TypeF]

  object TypeT {
    def apply(v: TypeF[TypeT]): TypeT = Fix(v)

    def unapply(v: TypeT): Option[TypeF[TypeT]] = Some(v.unfix)
  }

  type Module = ModuleF[TypeT]
}
