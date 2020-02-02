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
      types0 <- p.sortedTypes.fold[Result[List[TypeDef]]](error, ok)
      types <- types0.traverse(typeDoc)
      prefix <- getPrefixCode
      features <- features.collectFeatures(_.handlePackage(p))
    } yield
      Doc.intercalate(
        packageSep,
        comment.toList ++ List(packageName) ++ prefix.toList ++ features ++
          types.toNel.map(l => Doc.intercalate(typesSep, l.toList)).toList
      )

}
