package sculptor.scalagen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.scalagen.Feature

package object features {
  def collectFeatures(f: Feature => Result[List[Doc]]): Result[List[Doc]] =
    getFeatures.flatMap(_.flatTraverse(f))
}
