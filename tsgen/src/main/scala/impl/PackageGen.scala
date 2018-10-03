package sculptor.tsgen.impl

import cats.implicits._
import org.typelevel.paiges._

import sculptor.ast._

object PackageGen extends GenHelpers {

  private val packageSep = dblLine

  private val typesSep = dblLine

  private def typeDoc(t: TypeDef) =
    TypeDef.cata[Result[Doc]](
      NewtypeGen.generate,
      AliasGen.generate,
      RecordGen.generate,
      EnumGen.generate,
      ADTGen.generate
    )(t)

  def generate(p: Package): Result[Doc] =
    for {
      types <- p.types.traverse(typeDoc)
      prefix <- getPrefixCode
    } yield
      Doc.intercalate(
        packageSep,
        prefix.toList
          ++ types.toNel.map(l => Doc.intercalate(typesSep, l.toList)).toList
      )

}
