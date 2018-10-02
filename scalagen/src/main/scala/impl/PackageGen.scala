package sculptor.scalagen.impl

import cats.implicits._
import org.typelevel.paiges._

import sculptor.ast._

object PackageGen extends GenHelpers {

  private val packageSep = dblLine

  private val typesSep = dblLine

  private def packageDoc(name: FQName) =
    ok(Doc.text(s"""package ${name.mkString(".")}"""))

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
      packageName <- packageDoc(p.name)
      types <- p.types.traverse(typeDoc)
      prefix <- getPrefixCode
    } yield
      Doc.intercalate(
        packageSep,
        packageName
          :: prefix.toList
          ++ types.toNel.map(l => Doc.intercalate(typesSep, l.toList)).toList
      )

}
