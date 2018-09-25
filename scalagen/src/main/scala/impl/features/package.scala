package sculptor.scalagen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.scalagen.{Feature => FeatureConfig}

package object features {

  private val getFeature: FeatureConfig => Feature = {
    case FeatureConfig.CatsEqTypeclass => CatsEqTypeclass
  }

  def collectFeatures(f: Feature => Result[Option[Doc]]): Result[List[Doc]] =
    getFeatures.flatMap(
      _.traverse(v => f(getFeature(v))).map(_.map(_.toList).flatten)
    )

}
