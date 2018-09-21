package sculptor.scalagen.deprecated
package xsd

import cats._
import cats.implicits._
import cats.data.{NonEmptyList => NEL, _}
import scalax.collection.{State => _, _}, GraphEdge._
import scalax.collection.GraphPredef.EdgeLikeIn

import sculptor.xsd.{ast => x}
import ast._

object Transform {

  import Fold._

  type Dependencies = List[DiEdge[TypeRef.defined]]

  final case class TransformState(config: Config,
                                  fold: Fold,
                                  unparsedTypes: List[x.Type[SrcF]],
                                  dependencies: Dependencies)

  type Result[A] = EitherT[State[TransformState, ?], String, A]

  def ok[A](v: A): Result[A] = EitherT.rightT(v)
  def error[A](err: String): Result[A] = EitherT.leftT(err)
  def getTransformState: Result[TransformState] =
    EitherT.liftF(State.get[TransformState])
  def getFold: Result[Fold] =
    getTransformState.map(_.fold)
  def getConfig: Result[Config] = getTransformState.map(_.config)
  def setUnparsedTypes(t: List[x.Type[SrcF]]): Result[Unit] =
    EitherT.liftF(State.modify[TransformState](s => s.copy(unparsedTypes = t)))
  def pushUnparsedType(t: x.Type[SrcF]): Result[Unit] =
    EitherT.liftF(
      State.modify[TransformState](
        s => s.copy(unparsedTypes = t :: s.unparsedTypes)
      )
    )
  def popUnparsedType: Result[Option[x.Type[SrcF]]] =
    for {
      lst <- getTransformState.map(_.unparsedTypes)
      _ <- setUnparsedTypes(lst.drop(1))
    } yield lst.headOption
  def addDependency(`type`: TypeRef.defined,
                    toType: TypeRef.defined): Result[Unit] = {
    EitherT.liftF(
      State.modify[TransformState](
        // The order is reversed because we need descending topological sort
        s => s.copy(dependencies = DiEdge(toType, `type`) :: s.dependencies)
      )
    )
  }
  def getDependencies: Result[Dependencies] =
    getTransformState.map(_.dependencies)

  def splitCamelCase(v: String): List[String] = v match {
    case "" => Nil
    case _ if v.length === 1 => List(v)
    case _ =>
      if (v.forall(_.isUpper))
        List(v.toLowerCase.capitalize)
      else {
        val (x, xs) = v.drop(1).span(_.isLower)
        (v.take(1) + x) :: splitCamelCase(xs)
      }
  }

  def splitName(v: String): List[String] =
    v.replaceAll("\\.", "_").split('_').toList.map(splitCamelCase _).flatten

  def toCamelCase(v: String, firstUpperCase: Boolean): String = {
    def camel(firstUpper: Boolean, v: String): String =
      if (firstUpper) v.toLowerCase.capitalize
      else if (v.forall(_.isUpper)) v
      else v.toLowerCase
    splitName(v) match {
      case x :: xs => {
        val first = camel(firstUpperCase, x)
        (first :: xs.map(camel(true, _))).mkString
      }
      case _ => camel(firstUpperCase, v)
    }
  }

  def mkTypeName(v: String): String = toCamelCase(v, true)

  def enumName(v: String): String = mkTypeName(v)
  def enumMemberName(v: String): String = v.toList match {
    case x :: _ if Character.isJavaIdentifierStart(x) => v
    case _ => "V_" + v
  }

  def complexTypeName(v: String): String = mkTypeName(v)

  def newtypeTypeName(v: String): String = mkTypeName(v)

  def simpleTypeExtensionTypeName(v: String): String = mkTypeName(v)

  def fieldName(v: String): String = toCamelCase(v, false)

  def sortTypes(types: Map[TypeRef.defined, TypeDecl]): Result[List[TypeDecl]] =
    types.size match {
      case v if v < 2 => ok(types.values.toList)
      case _ =>
        for {
          deps <- getDependencies
          graph = Graph(deps: _*)
          sorted <- stableTopologicalSort(graph)
            .map(_.toList.flatMap(_._2.toList))
            .fold[Result[List[TypeDecl]]](
              _ => error(s"Found cyclic dependency: ${graph.findCycle}"),
              _.traverse { ref =>
                types
                  .get(ref)
                  .fold[Result[TypeDecl]](error(s"Can't sort type $ref"))(ok(_))
              }
            )
        } yield sorted
    }

  def schema(xsd: x.Schema[SrcF]): Result[ModuleDecl] =
    for {
      c <- getConfig
      _ <- setUnparsedTypes(xsd.types)
      t0 <- types()
      t <- sortTypes(t0)
    } yield
      ModuleDecl(ImportsDecl(c.imports).some, NEL.fromList(t).map(TypesDecl(_)))

  def unfoldM[M[_], A, B](
    seed: A
  )(f: A => M[Option[(A, B)]])(implicit M: Monad[M]): M[List[B]] =
    M.tailRecM((Nil: List[B], seed)) {
      case (acc, value) => {
        f(value) map {
          case None => Right(acc)
          case Some((value, a)) => Left((a :: acc, value))
        }
      }
    }

  def filterBadElementAliases(
    l: List[(TypeRef.defined, TypeDecl)]
  ): List[(TypeRef.defined, TypeDecl)] =
    l.filter {
      _ match {
        case (
            TypeRef.defined(Ident(name)),
            NewtypeDecl(_, TypeRef.defined(Ident(name1)), _)
            ) if name === name1 =>
          false
        case _ => true
      }
    }

  def types(): Result[Map[TypeRef.defined, TypeDecl]] =
    unfoldM(()) { _ =>
      popUnparsedType
        .map(_.traverse(`type`))
        .flatten
        .map(_.map(t => ((), (t.`type`, t))))
    }.map(filterBadElementAliases(_).toMap)

  lazy val `type`: x.Type[SrcF] => Result[TypeDecl] =
    x.Type.fold(simpleType, complexType, element)

  def annotationToComment(ann: Option[x.Annotation[SrcF]]): Option[Comment] =
    ann.flatMap(_.documentation.headOption)

  def simpleTypeEnum(
    t: x.SimpleType[SrcF]
  )(name: String, values: List[x.Enumeration[SrcF]]): Result[TypeDecl] =
    for {
      l <- ok(
        values
          .map(
            e =>
              EnumMemberDecl(
                Ident(enumMemberName(e.value)),
                e.value,
                annotationToComment(e.annotation)
            )
          )
      )
      members <- NEL
        .fromList(l)
        .fold(Errors.cantTransform[NEL[EnumMemberDecl]](t))(ok(_))
    } yield
      EnumDecl(
        TypeRef.definedFrom(enumName(name)),
        members,
        annotationToComment(t.annotation)
      )

  def simpleTypeNewtype(name: String,
                        base: x.QName,
                        ann: Option[x.Annotation[SrcF]]): Result[TypeDecl] = {
    val `type` =
      TypeRef.definedFrom(newtypeTypeName(name))
    for {
      baseRef <- typeRef(`type`)(base)
    } yield NewtypeDecl(`type`, baseRef, annotationToComment(ann))
  }

  def simpleType(t: x.SimpleType[SrcF]): Result[TypeDecl] =
    for {
      fold <- getFold
      result <- fold.simpleType[Result[TypeDecl]](
        simpleTypeEnum(t),
        simpleTypeNewtype,
        Errors.cantTransform
      )(t)
    } yield result

  def stdTypeRef(`type`: x.QName): Result[TypeRef] =
    for {
      config <- getConfig
      ns = config.xsdNs
      name <- if (`type`.ns === ns) {
        `type`.name match {
          case "string" => ok("String")
          case "base64Binary" => ok("String") // TODO: Custom type!
          case "anyURI" => ok("String") // TODO: Custom type!
          case "boolean" => ok("Boolean")
          case "int" => ok("Int")
          case "long" => ok("Long")
          case "integer" => ok("Int")
          case "nonNegativeInteger" => ok("Int")
          case "positiveInteger" => ok("Int")
          case "decimal" => ok("Double")
          case _ => Errors.unknownType[String](`type`)
        }
      } else Errors.unknownType[String](`type`)
    } yield TypeRef.std(Ident(name))

  def typeRef(forType: TypeRef.defined)(`type`: x.QName): Result[TypeRef] =
    for {
      config <- getConfig
      result <- config.externalTypes
        .find(_.xsdName === `type`)
        .fold[Result[TypeRef]](`type` match {
          case x.QName(_, ns) if ns === config.xsdNs =>
            stdTypeRef(`type`)
          case x.QName(t, None) => {
            val `type` =
              TypeRef.definedFrom(complexTypeName(t))
            for {
              _ <- addDependency(forType, `type`)
            } yield `type`
          }
          case _ => Errors.unknownType[TypeRef](`type`)
        })(t => ok(TypeRef.external(t.name)))
    } yield result

  object fieldConstraint {
    def unapply(
      v: (Option[Int], Option[Int], Boolean)
    ): Option[FieldConstraint] =
      v match {
        case (Some(0), Some(1), false) => FieldConstraint.Optional.some
        case (Some(0), Some(1), true) => FieldConstraint.OptionalNullable.some
        case (Some(1), Some(1), true) => FieldConstraint.Nullable.some
        case (Some(_), Some(_), false) => FieldConstraint.Required.some
        case (Some(_), Some(_), true) => FieldConstraint.Optional.some
        case (_, None, true) => FieldConstraint.Optional.some
        case (_, _, false) => FieldConstraint.Required.some
        case _ => none[FieldConstraint]
      }
  }

  object isArray {
    def unapply(v: (Option[Int], Option[Int])): Option[Boolean] =
      v match {
        case (_, Some(1)) => false.some
        case (_, Some(_)) => true.some
        case (_, None) => true.some
      }
  }

  def transformField(minOccurs: Option[Int],
                     maxOccurs: Option[Int],
                     nullable: Boolean)(f: FieldDecl): Result[FieldDecl] =
    for {
      fc <- (minOccurs, maxOccurs, nullable) match {
        case fieldConstraint(fc) => ok(fc)
        case _ =>
          Errors.cantHandleFieldConstraints[FieldConstraint](
            f,
            minOccurs,
            maxOccurs,
            nullable
          )
      }
      array <- (minOccurs, maxOccurs) match {
        case isArray(array) => ok(array)
        case _ =>
          Errors.cantHandleFieldConstraints[Boolean](
            f,
            minOccurs,
            maxOccurs,
            nullable
          )
      }
    } yield f.copy(constraint = fc, array = array)

  def bodyField(
    `type0`: TypeRef.defined
  )(body: x.Body[SrcF]): BodyFieldHandler[Result[List[FieldDecl]]] =
    (name, `type`, minOccurs, maxOccurs, nullable, ann) =>
      for {
        config <- getConfig
        t <- typeRef(`type0`)(
          `type`.getOrElse(x.QName("anyType", config.xsdNs))
        )
        result <- transformField(minOccurs, maxOccurs, nullable)(
          FieldDecl(
            Ident(fieldName(name)),
            name,
            t,
            FieldConstraint.Required,
            false,
            false,
            annotationToComment(ann)
          )
        )
      } yield List(result)

  def bodySequence(
    b: x.Body[SrcF],
    `type`: TypeRef.defined
  ): BodySequenceHandler[Result[List[FieldDecl]]] =
    (seq, minOccurs, maxOccurs) =>
      for {
        fields <- seq.body.extract.flatTraverse(bodyToFields(`type`))
        result <- fields.traverse(transformField(minOccurs, maxOccurs, false))
      } yield result

  def bodyChoice(
    b: x.Body[SrcF],
    `type`: TypeRef.defined
  ): BodyChoiceHandler[Result[List[FieldDecl]]] =
    (choice, minOccurs, maxOccurs) =>
      for {
        fields <- choice.body.extract.flatTraverse(bodyToFields(`type`))
        result <- fields.traverse(transformField(minOccurs, maxOccurs, false))
      } yield result.map(_.copy(constraint = FieldConstraint.Optional))

  def bodyAnonComplexType(
    b: x.Body[SrcF],
    `type`: TypeRef.defined
  ): BodyAnonComplexTypeHandler[Result[List[FieldDecl]]] =
    (name, ct, minOccurs, maxOccurs, nullable, ann) => {
      val anonTypeName = `type`.name.value + "_" + name
      for {
        _ <- pushUnparsedType(
          x.Type.complexType(ct.copy[SrcF](name = anonTypeName.some))
        )
        result <- bodyField(`type`)(b)(
          name,
          x.QName.fromString(anonTypeName).some,
          minOccurs,
          maxOccurs,
          nullable,
          ann
        )
      } yield result
    }

  def bodyAnonSimpleType(
    b: x.Body[SrcF],
    `type`: TypeRef.defined
  ): BodyAnonSimpleTypeHandler[Result[List[FieldDecl]]] =
    (name, st, minOccurs, maxOccurs, nullable, ann) => {
      val anonTypeName = `type`.name.value + "_" + name
      for {
        _ <- pushUnparsedType(
          x.Type.simpleType(st.copy[SrcF](name = anonTypeName.some))
        )
        result <- bodyField(`type`)(b)(
          name,
          x.QName.fromString(anonTypeName).some,
          minOccurs,
          maxOccurs,
          nullable,
          ann
        )
      } yield result
    }

  def bodyToFields(
    `type`: TypeRef.defined
  )(b: x.Body[SrcF]): Result[List[FieldDecl]] =
    for {
      folds <- getFold
      result <- folds.body(
        bodyField(`type`)(b),
        bodySequence(b, `type`),
        bodyChoice(b, `type`),
        bodyAnonComplexType(b, `type`),
        bodyAnonSimpleType(b, `type`),
        Errors.cantTransform
      )(b)
    } yield result

  def attrToField(
    `type`: TypeRef.defined
  )(t: x.AST[SrcF])(attr: x.Attribute[SrcF]): Result[FieldDecl] =
    attr match {
      case x.Attribute(_, Some(name), _, _, Some(typ), use) =>
        for {
          t <- typeRef(`type`)(x.QName.fromString(typ))
        } yield
          FieldDecl(
            Ident(fieldName(name)),
            name,
            t,
            use
              .map(_ === "optional")
              .fold[FieldConstraint](FieldConstraint.Required)(
                v =>
                  if (v) FieldConstraint.Optional else FieldConstraint.Required
              ),
            true,
            false,
            annotationToComment(attr.annotation)
          )
      case _ => Errors.cantTransform(t)
    }

  def attrsToFields(
    `type`: TypeRef.defined
  )(t: x.AST[SrcF])(attrs: List[x.Attribute[SrcF]]): Result[List[FieldDecl]] =
    attrs.traverse(attrToField(`type`)(t))

  def bodyListToType(
    t: x.ComplexType[SrcF],
    baseNameO: Option[x.QName],
    allOptional: Boolean
  )(name: String,
    b: List[x.Body[SrcF]],
    attrs: List[x.Attribute[SrcF]]): Result[TypeDecl] = {
    val `type` =
      TypeRef.definedFrom(complexTypeName(name))
    for {
      l0 <- b.flatTraverse(bodyToFields(`type`))
      l <- ok(
        if (allOptional) l0.map(_.copy(constraint = FieldConstraint.Optional))
        else l0
      )
      al <- attrsToFields(`type`)(t)(attrs)
      fields <- NEL
        .fromList(l ++ al)
        .fold(Errors.cantTransform[NEL[FieldDecl]](t))(ok(_))
      baseRef <- baseNameO.traverse(typeRef(`type`))
    } yield
      ComplexTypeDecl(
        `type`,
        baseRef,
        fields,
        annotationToComment(t.annotation)
      )
  }

  def complexTypeAny(name: String, any: x.Any[SrcF]): Result[TypeDecl] = {
    val type0 =
      TypeRef.definedFrom(newtypeTypeName(name))
    for {
      config <- getConfig
      `type` <- typeRef(type0)(x.QName("anyType", config.xsdNs))
    } yield NewtypeDecl(type0, `type`, annotationToComment(any.annotation))
  }

  def complexTypeSimpleContent(t: x.ComplexType[SrcF])(
    name: String,
    baseType: x.QName,
    attrs: List[x.Attribute[SrcF]]
  ): Result[TypeDecl] = {
    val type0 =
      TypeRef.definedFrom(simpleTypeExtensionTypeName(name))
    for {
      `type` <- typeRef(type0)(baseType)
      al <- attrsToFields(type0)(t)(attrs)
      fields <- NEL
        .fromList(al)
        .fold(Errors.cantTransform[NEL[FieldDecl]](t))(ok(_))
    } yield
      SimpleTypeExtensionDecl(
        type0,
        `type`,
        fields,
        annotationToComment(t.annotation)
      )
  }

  def complexType(t: x.ComplexType[SrcF]): Result[TypeDecl] =
    getFold flatMap { fold =>
      fold
        .complexType[Result[TypeDecl]](
          (name, bName, seq, attrs) =>
            bodyListToType(t, bName, false)(name, seq.body, attrs),
          (name, ch, attrs) =>
            bodyListToType(t, None, true)(name, ch.body, attrs),
          complexTypeAny,
          simpleTypeNewtype,
          complexTypeSimpleContent(t),
          Errors.cantTransform
        )(t)
    }

  def element(t: x.Element[SrcF]): Result[TypeDecl] =
    getFold flatMap {
      _.element[Result[TypeDecl]](
        (name, ct) => complexType(ct.copy[SrcF](name = Some(name))),
        simpleTypeNewtype,
        Errors.cantTransform
      )(t)
    }

  private def stableTopologicalSort[E[X] <: EdgeLikeIn[X]](
    graph: Graph[TypeRef.defined, E]
  ): Either[graph.NodeT, graph.LayeredTopologicalOrder[graph.NodeT]] = {
    val compareByName = graph.NodeOrdering { (a, b) =>
      String.CASE_INSENSITIVE_ORDER
        .compare(a.value.name.value, b.value.name.value)
    }

    graph.topologicalSort
      .map { ts =>
        ts.toLayered
          .withLayerOrdering(compareByName)
      }
  }
}
