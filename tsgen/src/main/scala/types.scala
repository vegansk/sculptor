package sculptor.tsgen

import org.typelevel.paiges._

sealed trait Feature

object Feature {

  case object IoTsTypes extends Feature
  case object Constructors extends Feature

}

final case class OptionalEncoding(optionalType: String = "",
                                  allFieldsOptional: Boolean = false)

final case class Config(tabSize: Int = 2,
                        features: List[Feature] = List.empty,
                        lineWidth: Int = 120,
                        adtTag: String = "",
                        prefixCode: String = "",
                        optionalEncoding: OptionalEncoding = OptionalEncoding())

object Config {
  def tabSize(c: Config): Int = c.tabSize
  def features(c: Config): List[Feature] =
    Map(c.features.map(f => (f.getClass, f)): _*).values.toList
  def lineWidth(c: Config): Int = c.lineWidth
  def adtTag(c: Config): String = if (c.adtTag.isEmpty()) "__tag" else c.adtTag
  def prefixCode(c: Config): Option[Doc] =
    Option(c.prefixCode).filterNot(_.isEmpty).map(Doc.text)
  def optionalEncoding(c: Config): Option[OptionalEncoding] =
    Option(c.optionalEncoding)
      .filter(c => !c.optionalType.isEmpty || c.allFieldsOptional)
}
