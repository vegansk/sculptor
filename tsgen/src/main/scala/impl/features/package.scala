package sculptor.tsgen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.tsgen.{Feature => FeatureConfig}

package object features {

  private val getFeature: FeatureConfig => Feature = {
    case v: FeatureConfig.IoTsTypes => IoTsTypes(v)
    case FeatureConfig.Constructors => Constructors
  }

  def collectFeatures(f: Feature => Result[List[Doc]]): Result[List[Doc]] =
    getFeatures.flatMap(_.traverse(v => f(getFeature(v))).map(_.flatten))

}
