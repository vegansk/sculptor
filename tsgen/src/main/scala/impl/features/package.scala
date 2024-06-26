package sculptor.tsgen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast.TypeDef
import sculptor.tsgen.{Feature => FeatureConfig}

package object features {

  private val getFeature: FeatureConfig => Feature = {
    case v: FeatureConfig.IoTsTypes => IoTsTypes(v)
    case FeatureConfig.Constructors => Constructors
    case FeatureConfig.AdditionalCode => AdditionalCode
  }

  def collectTypeFeatures(
    typeDef: TypeDef
  )(f: Feature => Result[List[Doc]]): Result[List[Doc]] =
    getTypeFeatures(typeDef.name.name)
      .flatMap(_.flatTraverse(v => f(getFeature(v))))

  def collectFeatures(f: Feature => Result[List[Doc]]): Result[List[Doc]] =
    getFeatures.flatMap(_.flatTraverse(v => f(getFeature(v))))

}
