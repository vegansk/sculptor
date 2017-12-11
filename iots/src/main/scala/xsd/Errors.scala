package sculptor
package iots
package xsd

import sculptor.xsd.{ast => x}
import ast._

object Errors {

  import Transform._

  def cantTransform[A](v: x.AST[SrcF]): Result[A] = v match {
    case st: x.SimpleType[SrcF] =>
      error(
        s"""Can't transform simple type `${st.name.getOrElse("<unknown>")}`"""
      )
    case ct: x.ComplexType[SrcF] =>
      error(s"""Can't transform complex type $ct""")
    // error(
    //   s"""Can't transform complex type `${ct.name.getOrElse("<unknown>")}`"""
    // )
    case e: x.Element[SrcF] =>
      error(s"""Can't transform element `${e.name.getOrElse("<unknown>")}`""")
    case _ =>
      error(s"""Can't transform AST node $v""")
    // error(s"""Can't transform AST node of type `${v.getClass.getName
    //   .split("\\$")
    //   .last}`""")
  }

  def unknownType[A](t: x.QName): Result[A] = error(s"""Unknown type "$t"""")

  def cantHandleFieldConstraints[A](f: FieldDecl,
                                    minOccurs: Option[Int],
                                    maxOccurs: Option[Int],
                                    nullable: Boolean): Result[A] =
    error(
      s"""Can't handle constraints for field ${f.name.value}: min: ${minOccurs}, max: ${maxOccurs}, nullable: ${nullable}"""
    )

  def cantExtendType[A](t: x.QName): Result[A] =
    error(s"""Can't extend type $t""")

}
