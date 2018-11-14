package sculptor.common

import org.typelevel.paiges._

import sculptor.ast._

trait CommonGenHelpers {

  def typeDefKindName(typ: TypeDef): String =
    TypeDef.cata(
      _ => "Newtype",
      _ => "Alias",
      _ => "Record",
      _ => "Enum",
      _ => "ADT"
    )(typ)

  def typeComment(typ: TypeDef, typDoc: Doc): Doc = {
    Doc.text(s"// ${typeDefKindName(typ)} ") + typDoc + typ.comment.fold(
      Doc.empty
    )(c => Doc.text(s": $c"))
  }

  def optionalComment(
    genComment: Boolean
  )(comment: => Option[String]): Option[Doc] =
    Option(genComment)
      .filter(identity)
      .flatMap(_ => comment)
      .map(c => Doc.text(s"// $c"))

  def comment(genComment: Boolean)(comment: => String): Option[Doc] =
    Option(genComment).filter(identity).map(_ => Doc.text(s"// $comment"))

  def doc(needDoc: Boolean)(doc: => Doc): Option[Doc] =
    Option(needDoc).filter(identity).map(_ => doc)

  def optionalDoc(needDoc: Boolean)(doc: => Option[Doc]): Option[Doc] =
    Option(needDoc).filter(identity).flatMap(_ => doc)

}
