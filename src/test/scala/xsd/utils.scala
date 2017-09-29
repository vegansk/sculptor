package sculptor.xsd

import scala.xml._
import org.specs2.matcher.MustMatchers

object utils extends MustMatchers {

  def xsd(n: Node): Node = {
    <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" elementFormDefault="qualified">
      {n}
    </xs:schema>
  }

  def runState[A](s: fold.FoldState)(r: fold.Result[A]): (fold.FoldState, Either[String,A]) =
    r.value.run(s).value

  def run[A](r: fold.Result[A]): (fold.FoldState, Either[String,A]) =
    runState(fold.FoldState(appendPathToError = true, strictMode = true))(r)

  def checkFold[A](initial: A, expected: A)(op: fold.SchemaOp[A])(data: Node) = {
    val res = run {
      op(data)(initial)
    }

    res._2 must_=== Right(expected)
  }

  def checkParser[A](expected: A)(r: fold.Result[A]) = {
    val res = run(r)

    res._2 must_=== Right(expected)
  }

}
