package sculptor

import cats._
import cats.data._
import cats.implicits._

import scala.xml._

package xsd {
  import ast.Schema

  final case class ParseResult(schemaNs: Option[String], ast: Schema[Id])
}

package object xsd {

  def parseSchema(n: Node): ValidatedNel[String, ParseResult] = {
    val (state, res) = parser[Option]
      .parse(n)
      .value
      .run(fold.FoldState())
      .value

    res.toValidatedNel
      .andThen(ast.Schema.build(_).map(ParseResult(state.schemaNs, _)))
  }

}
