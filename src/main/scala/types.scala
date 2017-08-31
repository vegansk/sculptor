package sculptor

object types {

  type Num = Int

  sealed trait Type

  sealed trait Standard extends Type

  sealed trait Ordinal extends Standard

  sealed trait StringLike extends Standard

  final case object Int extends Ordinal

  final case object Str extends StringLike

  /** Restricted string type */
  final case class RStr(
    name: Option[String] = None,
    minLength: Option[Num] = None,
    maxLength: Option[Num] = None,
    regExp: List[String] = Nil
  ) extends StringLike

  final case class ROrdinal(
    name: Option[String] = None,
    `type`: Ordinal = Int,
    minInclusive: Option[Num] = None,
    maxInclusive: Option[Num] = None,
    minExclusive: Option[Num] = None,
    maxExclusive: Option[Num] = None,
    totalDigits: Option[Num] = None,
    regExp: List[String] = Nil
  ) extends Ordinal

  final case class Record(name: Option[String], fields: Map[String, Type]) extends Type

  final case class Module(name: Option[String], types: List[Type])
}
