package sculptor.scalagen

import org.typelevel.paiges._

import sculptor.ast._

trait Feature {
  def handleNewtype(n: Newtype): impl.Result[List[Doc]] = impl.ok(List.empty)

  def handleAlias(a: Alias): impl.Result[List[Doc]] = impl.ok(List.empty)

  def handleRecord(r: Record): impl.Result[List[Doc]] = impl.ok(List.empty)

  def handleADT(a: ADT): impl.Result[List[Doc]] = impl.ok(List.empty)

  def handleEnum(e: Enum): impl.Result[List[Doc]] = impl.ok(List.empty)

  def handlePackage(p: Package): impl.Result[List[Doc]] = impl.ok(List.empty)
}

object Feature {
  val CatsEqTypeclass: Feature = impl.features.CatsEqTypeclass
  def CirceCodecs(adtTag: String = ""): Feature =
    new impl.features.CirceCodecs(adtTag = adtTag)
  val AdditionalCode: Feature = impl.features.AdditionalCode
  def TapirSchema(adtTag: String = "",
                  schemaLazyInstances: Boolean = false): Feature =
    new impl.features.TapirSchema(
      adtTag = adtTag,
      schemaLazyInstances = schemaLazyInstances
    )
}

final case class TypesFeaturesOverrides(
  value: Map[String, List[Feature]] = Map.empty
) extends AnyVal

final case class Config(tabSize: Int = 2,
                        features: List[Feature] = List.empty,
                        typesFeaturesOverrides: TypesFeaturesOverrides =
                          TypesFeaturesOverrides(),
                        lineWidth: Int = 120,
                        prefixCode: String = "",
                        generateComments: Boolean = true,
                        generateAdtConstructorsHelpers: Boolean = true,
                        generateEnumsDescriptions: Boolean = false,
                        generateParametersDefaultValues: Boolean = false)

object Config {
  def tabSize(c: Config): Int = c.tabSize
  def features(c: Config): List[Feature] = c.features
  def typesFeaturesOverrides(c: Config): TypesFeaturesOverrides =
    c.typesFeaturesOverrides
  def lineWidth(c: Config): Int = c.lineWidth
  def prefixCode(c: Config): Option[Doc] =
    Option(c.prefixCode).filterNot(_.isEmpty).map(Doc.text)
  def generateComments(c: Config): Boolean =
    c.generateComments
  def generateAdtConstructorsHelpers(c: Config): Boolean =
    c.generateAdtConstructorsHelpers
  def generateEnumDescriptions(c: Config): Boolean =
    c.generateEnumsDescriptions
  def generateParametersDefaultValues(c: Config): Boolean =
    c.generateParametersDefaultValues
}
