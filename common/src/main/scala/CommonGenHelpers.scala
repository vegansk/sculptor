package sculptor.common

import org.typelevel.paiges._

import sculptor.ast._

trait CommonGenHelpers {

  def decapitalize(s: String): String = {
    val (x, xs) = s.splitAt(1)
    x.toLowerCase + xs
  }

  def typeDefKindName(typ: TypeDef): String =
    TypeDef.cata(
      _ => "Newtype",
      _ => "Alias",
      _ => "Record",
      _ => "Enum",
      _ => "ADT"
    )(typ)

  def typeComment(typ: TypeDef, typDoc: Doc): Doc = {
    val header = Doc.text(s"// ${typeDefKindName(typ)} ") + typDoc

    typ.comment match {
      case None => header
      case Some(c) =>
        Doc.stack(
          (header + Doc.text(":")) +:
            c.linesIterator.map(line => Doc.text(s"// $line")).toVector
        )
    }
  }

  def optionalComment(
    genComment: Boolean
  )(comment: => Option[String]): Option[Doc] =
    Option(genComment)
      .filter(identity)
      .flatMap(_ => comment)
      .map(c => Doc.text(s"/* $c */"))

  def comment(genComment: Boolean)(comment: => String): Option[Doc] =
    Option(genComment).filter(identity).map(_ => Doc.text(s"/* $comment */"))

  def doc(needDoc: Boolean)(doc: => Doc): Option[Doc] =
    Option(needDoc).filter(identity).map(_ => doc)

  def optionalDoc(needDoc: Boolean)(doc: => Option[Doc]): Option[Doc] =
    Option(needDoc).filter(identity).flatMap(_ => doc)

  val spacePrefix: Doc => Doc = Doc.char(' ') + _

}
