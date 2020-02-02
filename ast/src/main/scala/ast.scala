package sculptor
package ast

import cats.data._
import cats.implicits._
import org.typelevel.paiges.Doc
import scalax.collection.{State => _, _}, GraphEdge._
import scalax.collection.GraphPredef.EdgeLikeIn

/** Identifier representation */
final case class Ident(name: String) extends AnyVal

/** Full qualified name */
final case class FQName(name: Ident, prefix: List[Ident] = Nil) {
  def mkString(sep: String): String =
    (prefix ++ List(name)).map(_.name).mkString(sep)
}

object FQName {
  def of(name: String): FQName = {
    name.split("\\.").toList.toNel.fold(FQName(Ident(name))) { l =>
      val names = l.map(Ident(_))
      FQName(names.last, names.init)
    }
  }
}

/** Types references ADT, used in type definitions */
sealed trait TypeRef {
  def asString: String
  def `type`: Option[TypeDef]
}

object TypeRef {

  /** Specialized type reference
    *
    * @param name       Name of the type
    * @param parameters Generic parameters
    * @param type       Link to the real type to support dependencies
    */
  final case class Specialized(name: FQName,
                               parameters: List[TypeRef] = Nil,
                               `type`: Option[TypeDef] = None)
      extends TypeRef {
    def asString = name.mkString(".")
  }

  /** Generic type reference */
  final case class Generic(name: Ident) extends TypeRef {
    def asString = name.name
    val `type` = None
  }

  def spec(name: String, parameters: TypeRef*): Specialized =
    spec0(FQName.of(name), None, parameters.toList)

  def spec0(name: FQName,
            `type`: Option[TypeDef],
            parameters: List[TypeRef]): Specialized =
    Specialized(name, parameters, `type`)

  def gen(name: String): Generic = Generic(Ident(name))

  def cata[A](specialized: Specialized => A,
              generic: Generic => A)(t: TypeRef): A = t match {
    case s: Specialized => specialized(s)
    case g: Generic => generic(g)
  }
}

/** Generic parameter definition */
final case class GenericDef(`type`: TypeRef.Generic,
                            `extends`: List[TypeRef] = Nil)

object GenericDef {
  implicit def genericDefFromGeneric(g: TypeRef.Generic): GenericDef =
    GenericDef(g)

  def of(name: String, `extends`: TypeRef*): GenericDef =
    GenericDef(TypeRef.gen(name), `extends`.toList)
}

/** Types definitions ADT */
sealed trait TypeDef {
  def name: Ident
  def comment: Option[String]
  def ref: TypeRef
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def ref(parameters: TypeRef*): TypeRef
  def isNewtype: Boolean = false
  def isAlias: Boolean = false
  def isRecord: Boolean = false
  def isEnum: Boolean = false
  def isADT: Boolean = false
  def additionalCode: Option[NonEmptyList[Doc]]

  def dependencies: List[TypeDef] =
    TypeDef.cata[List[TypeDef]](
      _.baseType.`type`.toList,
      _.baseType.`type`.toList,
      _.fields.toList.map(_.`type`.`type`).flattenOption,
      _ => Nil,
      _.constructors.map(_.fields.map(_.`type`.`type`).flattenOption).combineAll
    )(this)
}

object TypeDef {

  def cata[A](newtype: Newtype => A,
              alias: Alias => A,
              record: Record => A,
              enum: Enum => A,
              adt: ADT => A): TypeDef => A = {
    case t: Newtype => newtype(t)
    case t: Alias => alias(t)
    case t: Record => record(t)
    case t: Enum => enum(t)
    case t: ADT => adt(t)
  }

}

/** Validator */
sealed trait Validator

final case class ValidationFunction(name: String) extends Validator

/** Newtype definition */
final case class Newtype(name: Ident,
                         parameters: List[GenericDef],
                         baseType: TypeRef,
                         comment: Option[String] = None,
                         validator: Option[Validator] = None,
                         additionalCode: Option[NonEmptyList[Doc]] = None)
    extends TypeDef {
  lazy val ref: TypeRef =
    ref(parameters.map(_.`type`): _*)

  def ref(parameters: TypeRef*): TypeRef =
    TypeRef.spec0(FQName(name), this.some, parameters.toList)

  override def isNewtype = true
}

/** Alias */
final case class Alias(name: Ident,
                       parameters: List[GenericDef],
                       baseType: TypeRef,
                       comment: Option[String] = None,
                       additionalCode: Option[NonEmptyList[Doc]] = None)
    extends TypeDef {
  lazy val ref: TypeRef =
    ref(parameters.map(_.`type`): _*)

  def ref(parameters: TypeRef*): TypeRef =
    TypeRef.spec0(FQName(name), this.some, parameters.toList)

  override def isAlias = true
}

/** Field definition */
final case class FieldDef(name: Ident,
                          `type`: TypeRef,
                          comment: Option[String] = None,
                          validator: Option[Validator] = None)

/** Record */
final case class Record(name: Ident,
                        parameters: List[GenericDef],
                        fields: NonEmptyList[FieldDef],
                        comment: Option[String] = None,
                        validator: Option[Validator] = None,
                        additionalCode: Option[NonEmptyList[Doc]] = None)
    extends TypeDef {
  lazy val ref: TypeRef =
    ref(parameters.map(_.`type`): _*)

  def ref(parameters: TypeRef*): TypeRef =
    TypeRef.spec0(FQName(name), this.some, parameters.toList)

  override def isRecord = true
}

/** Enumeration value */
final case class EnumValue(name: Ident,
                           private[ast] val value0: Option[String] = None,
                           comment: Option[String] = None) {
  def value: String = value0.getOrElse(name.name)
}

/** Enumeration */
final case class Enum(name: Ident,
                      values: NonEmptyList[EnumValue],
                      comment: Option[String] = None,
                      additionalCode: Option[NonEmptyList[Doc]] = None)
    extends TypeDef {
  lazy val ref: TypeRef = TypeRef.spec0(FQName(name), this.some, Nil)

  def ref(parameters: TypeRef*): TypeRef = ref

  override def isEnum = true
}

/** ADT constructor definition */
final case class ADTConstructor(name: Ident,
                                parameters: List[GenericDef],
                                fields: List[FieldDef],
                                comment: Option[String] = None,
                                tag: Option[String] = None) {
  def ref: TypeRef =
    TypeRef.spec0(FQName.of(name.name), None, parameters.map(_.`type`))
}

/** ADT definition */
final case class ADT(name: Ident,
                     parameters: List[GenericDef],
                     constructors: NonEmptyList[ADTConstructor],
                     comment: Option[String] = None,
                     validator: Option[Validator] = None,
                     additionalCode: Option[NonEmptyList[Doc]] = None)
    extends TypeDef {
  lazy val ref: TypeRef =
    ref(parameters.map(_.`type`): _*)

  def ref(parameters: TypeRef*): TypeRef =
    TypeRef.spec0(FQName(name), this.some, parameters.toList)

  override def isADT = true
}

/** Types package */
final case class Package(name: FQName,
                         types: List[TypeDef],
                         comment: Option[String] = None,
                         additionalCode: Option[NonEmptyList[Doc]] = None) {
  def sortedTypes: Either[String, List[TypeDef]] = {
    val dependencies: List[DiEdge[TypeDef]] =
      types.map(t => DiEdge(t, t) :: t.dependencies.map(DiEdge(_, t))).flatten
    val graph = Graph(dependencies: _*)
    stableTopologicalSort(graph)
      .map(_.toList.map(_._2.toList))
      .fold[Either[String, List[TypeDef]]](
        _ => s"Found cyclic dependency: ${graph.findCycle}".asLeft,
        _.value.flatten.map(_.value).asRight
      )
  }

  private def stableTopologicalSort[E[X] <: EdgeLikeIn[X]](
    graph: Graph[TypeDef, E]
  ): Either[graph.NodeT, graph.LayeredTopologicalOrder[graph.NodeT]] = {
    val compareByName = graph.NodeOrdering { (a, b) =>
      String.CASE_INSENSITIVE_ORDER
        .compare(a.value.name.name, b.value.name.name)
    }

    graph.topologicalSort
      .map { ts =>
        ts.toLayered
          .withLayerOrdering(compareByName)
      }
  }
}
