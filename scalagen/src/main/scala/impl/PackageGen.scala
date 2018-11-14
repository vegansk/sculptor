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
      genComments <- getGenerateComments
      comment = Option(genComments)
        .filter(identity)
        .flatMap(_ => p.comment)
        .map(c => Doc.text(s"/*\n${c}\n*/"))
      packageName <- packageDoc(p.name)
      types <- p.types.traverse(typeDoc)
      prefix <- getPrefixCode
    } yield
      Doc.intercalate(
        packageSep,
        comment.toList ++ List(packageName) ++ prefix.toList ++
          types.toNel.map(l => Doc.intercalate(typesSep, l.toList)).toList
      )

}
