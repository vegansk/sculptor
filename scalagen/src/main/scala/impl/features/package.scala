package sculptor.scalagen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast.TypeDef
import sculptor.scalagen.Feature

package object features {
  def collectTypeFeatures(
    typeDef: TypeDef
  )(f: Feature => Result[List[Doc]]): Result[List[Doc]] =
    getTypeFeatures(typeDef.name.name)
      .flatMap(_.flatTraverse(f))

  def collectFeatures(f: Feature => Result[List[Doc]]): Result[List[Doc]] =
    getFeatures.flatMap(_.flatTraverse(f))
}
