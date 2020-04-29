package sculptor.scalagen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.scalagen.{Feature => FeatureConfig}

package object features {

  private val getFeature: FeatureConfig => Feature = {
    case v: FeatureConfig.CirceCodecs => new CirceCodecs(v)
    case FeatureConfig.CatsEqTypeclass => CatsEqTypeclass
    case FeatureConfig.AdditionalCode => AdditionalCode
    case FeatureConfig.TapirSchema => TapirSchema
  }

  def collectFeatures(f: Feature => Result[List[Doc]]): Result[List[Doc]] =
    getFeatures.flatMap(_.traverse(v => f(getFeature(v))).map(_.flatten))

}
