package sculptor.scalagen.impl

import org.typelevel.paiges._
import cats.implicits._

import sculptor.ast._

object NewtypeGen extends GenHelpers {

  def generate(n: Newtype): Result[Doc] =
    for {
      indent <- getIndent

      typ = createTypeExpr0(n.name.name, n.parameters.map(createGenericParam))

      genComments <- getGenerateComments

      comment = Option(genComments)
        .filter(identity)
        .map(_ => typeComment(n, typ))

      valueType = createTypeRef(n.baseType)

      prefix = Doc.text("final case class ") + typ + Doc.char('(')

      body = Doc.text("value: ") + valueType

      newtype = body.tightBracketBy(prefix, newtypePostfix, indent)

      features <- features.collectFeatures(_.handleNewtype(n))

      result = comment.toList ++ List(newtype) ++ features.toNel.map { f =>
        val prefix = objectPrefix(createTypeExpr(n.name.name, Nil))
        Doc
          .intercalate(dblLine, f.toList)
          .tightBracketBy(prefix, objectPostfix, indent)
      }.toList
    } yield Doc.intercalate(dblLine, result)
}
