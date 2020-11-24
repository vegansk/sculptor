package sculptor.tsgen

import org.typelevel.paiges._
import cats.implicits._

sealed trait Feature

object Feature {

  final case class IoTsTypes(iotsNs: String = "",
                             typePrefix: String = "",
                             typeEnding: String = "Type",
                             typeMapping: IotsMapping = StdIotsMappings,
                             customIotsType: String = "",
                             customIotsTaggedType: String = "",
                             codecsObjectEnding: String = "Codecs")
      extends Feature
  case object Constructors extends Feature
  case object AdditionalCode extends Feature

  object IoTsTypes {

    def iotsNsPrefix(cfg: IoTsTypes): String =
      Option(cfg.iotsNs).filterNot(_.isEmpty).map(ns => s"${ns}.").orEmpty

  }

}

final case class OptionalEncoding(optionalType: String = "",
                                  allFieldsOptional: Boolean = false)

final case class Config(tabSize: Int = 2,
                        features: List[Feature] = List.empty,
                        lineWidth: Int = 120,
                        adtTag: String = "",
                        prefixCode: String = "",
                        optionalEncoding: OptionalEncoding = OptionalEncoding(),
                        generateComments: Boolean = true,
                        generateAdtNs: Boolean = true,
                        generateEnumsDescriptions: Boolean = false)

object Config {
  def tabSize(c: Config): Int = c.tabSize
  def features(c: Config): List[Feature] = c.features
  def lineWidth(c: Config): Int = c.lineWidth
  def adtTag(c: Config): String = if (c.adtTag.isEmpty()) "__tag" else c.adtTag
  def prefixCode(c: Config): Option[Doc] =
    Option(c.prefixCode).filterNot(_.isEmpty).map(Doc.text)
  def optionalEncoding(c: Config): Option[OptionalEncoding] =
    Option(c.optionalEncoding)
      .filter(c => !c.optionalType.isEmpty || c.allFieldsOptional)
  def generateComments(c: Config): Boolean =
    c.generateComments
  def generateAdtNs(c: Config): Boolean =
    c.generateAdtNs
  def generateEnumsDescriptions(c: Config): Boolean =
    c.generateEnumsDescriptions
}
