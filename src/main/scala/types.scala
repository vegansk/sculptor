package sculptor

object types {

  type Number = Int

  sealed trait TypeF[A]

  final case class IntF[A]() extends TypeF[A]

  final case class StringF[A]() extends TypeF[A]

  final case class RestrictedStringF[A](name: Option[String] = None,
                                        baseType: A,
                                        minLength: Option[Number] = None,
                                        maxLength: Option[Number] = None,
                                        regExp: List[String] = Nil)
      extends TypeF[A]

  final case class RestrictedNumberF[A](name: Option[String] = None,
                                        baseType: A,
                                        minInclusive: Option[Number] = None,
                                        maxInclusive: Option[Number] = None,
                                        minExclusive: Option[Number] = None,
                                        maxExclusive: Option[Number] = None,
                                        totalDigits: Option[Number] = None,
                                        regExp: List[String] = Nil)
      extends TypeF[A]

  final case class RecordF[A](name: Option[String], fields: List[(String, A)])
      extends TypeF[A]

  final case class ModuleF[A](name: Option[String], types: List[A])

  type TypeT = Fix[TypeF]

  object TypeT { def apply(a: TypeF[TypeT]): TypeT = Fix(a) }

  type Module = ModuleF[TypeT]
}
