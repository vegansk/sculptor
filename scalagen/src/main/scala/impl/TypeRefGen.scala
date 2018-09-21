package sculptor.scalagen.impl

import org.typelevel.paiges._

import sculptor.ast._

object TypeRefGen extends GenHelpers {

  def generate(r: TypeRef): Result[Doc] = ok(createTypeRef(r))
}
