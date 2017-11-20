package sculptor

import cats._
import cats.data._
import cats.implicits._

import scala.xml._

package object xsd {

  def schemaAst(n: Node): ValidatedNel[String, ast.Schema[Id]] = {
    parser[Option]
      .parse(n)
      .value
      .run(fold.FoldState())
      .value
      ._2
      .toValidatedNel
      .andThen(ast.Schema.build(_))
  }

}
