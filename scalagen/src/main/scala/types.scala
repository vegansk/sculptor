package sculptor.scalagen

import org.typelevel.paiges._

sealed trait Feature

object Feature {

  case object CatsEqTypeclass extends Feature
  final case class CirceCodecs(adtTag: String = "") extends Feature

}

final case class Config(tabSize: Int = 2,
                        features: List[Feature] = List.empty,
                        lineWidth: Int = 120,
                        prefixCode: String = "",
                        generateComments: Boolean = true,
                        generateAdtConstructorsHelpers: Boolean = true)

object Config {
  def tabSize(c: Config): Int = c.tabSize
  def features(c: Config): List[Feature] =
    Map(c.features.map(f => (f.getClass, f)): _*).values.toList
  def lineWidth(c: Config): Int = c.lineWidth
  def prefixCode(c: Config): Option[Doc] =
    Option(c.prefixCode).filterNot(_.isEmpty).map(Doc.text)
  def generateComments(c: Config): Boolean =
    c.generateComments
  def generateAdtConstructorsHelpers(c: Config): Boolean =
    c.generateAdtConstructorsHelpers
}
