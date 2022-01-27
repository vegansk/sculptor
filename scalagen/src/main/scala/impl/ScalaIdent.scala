package sculptor.scalagen.impl

import sculptor.ast.Ident
import sculptor.ast.FQName

object ScalaIdent {

  private def isReservedWord(ident: String): Boolean =
    List(
      "abstract",
      "case",
      "catch",
      "class",
      "def",
      "do",
      "else",
      "extends",
      "false",
      "final",
      "finally",
      "for",
      "forSome",
      "if",
      "implicit",
      "import",
      "lazy",
      "macro",
      "match",
      "new",
      "null",
      "object",
      "override",
      "package",
      "private",
      "protected",
      "return",
      "sealed",
      "super",
      "this",
      "throw",
      "trait",
      "try",
      "true",
      "type",
      "val",
      "var",
      "while",
      "with",
      "yield"
    ).contains(ident)

  private def escape(ident: String): String = {
    ident match {
      case i if isReservedWord(i) => s"`$i`"
      case _ => ident
    }
  }

  implicit class IdentAsScala(ident: Ident) {
    def asScalaId: String = escape(ident.name)
  }

  implicit class FQNameAsScala(name: FQName) {
    def asScalaId: String =
      (name.prefix ++ List(name.name)).map(_.asScalaId).mkString(".")
  }
}
