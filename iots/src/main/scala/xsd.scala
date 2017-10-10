package sculptor
package iots

import cats._
import cats.data._
import cats.implicits._

import shapeless.{Id => _, _}

object xsd {

  import ast._
  import sculptor.xsd.{ast => x}

  type Result[A] = ValidatedNel[String, A]

  private val iotsPackage = Ident("t", None)

  private def pure[A](a: A): Result[A] = Validated.validNel(a)
  private def error[A](err: String): Result[A] = Validated.invalidNel(err)

  private def foldComplexType(ct: x.ComplexType[Id]): Result[Type] =
    (ct.name, ct.complexContent, ct.sequence, ct.choice, ct.attributes) match {
      case (Some(n), None, None, None, List()) =>
        pure(
          Coproduct[Type](
            TypeAlias(
              n,
              Ident("undefined", iotsPackage.some),
              Ident("undefined").some
            )
          )
        )
      case _ => error[Type](s"Can't parse complex type $ct")
    }

  private object foldType extends Poly1 {
    implicit val atSimpleType: Case.Aux[x.SimpleType[Id], Result[Type]] = at(
      _ => error[Type]("Not implemented")
    )
    implicit val atComplexType: Case.Aux[x.ComplexType[Id], Result[Type]] = at(
      foldComplexType(_)
    )
    implicit val atElement: Case.Aux[x.Element[Id], Result[Type]] = at(
      _ => error[Type]("Not implemented")
    )
  }

  private def foldSchema(schema: x.Schema[Id]): Result[Module] =
    (schema.types: List[x.Type[Id]])
      .traverse(_.fold(foldType))
      .fold(Validated.invalid(_), types => Validated.validNel(Module(types)))

  def foldModule(ast: x.AST[Id]): Result[Module] = ast match {
    case schema: x.Schema[Id] => foldSchema(schema)
    case _ => Validated.invalidNel("iots.xsd.fold: Expected xsd.Schema")
  }

}
