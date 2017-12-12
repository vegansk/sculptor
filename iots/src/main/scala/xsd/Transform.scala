package sculptor
package iots
package xsd

import cats._
import cats.implicits._
import cats.data.{NonEmptyList => NEL, _}

import sculptor.xsd.{ast => x}
import ast._

object Transform {

  import Fold._

  final case class TransformState(config: Config,
                                  fold: Fold,
                                  unparsedTypes: List[x.Type[SrcF]] = Nil)

  type Result[A] = EitherT[State[TransformState, ?], String, A]

  def ok[A](v: A): Result[A] = EitherT.rightT(v)
  def error[A](err: String): Result[A] = EitherT.leftT(err)
  def getTransformState: Result[TransformState] =
    EitherT.liftT(State.get[TransformState])
  def getFold: Result[Fold] =
    getTransformState.map(_.fold)
  def getConfig: Result[Config] = getTransformState.map(_.config)
  def setUnparsedTypes(t: List[x.Type[SrcF]]): Result[Unit] =
    EitherT.liftT(State.modify[TransformState](s => s.copy(unparsedTypes = t)))
  def pushUnparsedType(t: x.Type[SrcF]): Result[Unit] =
    EitherT.liftT(
      State.modify[TransformState](
        s => s.copy(unparsedTypes = t :: s.unparsedTypes)
      )
    )
  def popUnparsedType: Result[Option[x.Type[SrcF]]] =
    for {
      lst <- getTransformState.map(_.unparsedTypes)
      _ <- setUnparsedTypes(lst.drop(1))
    } yield lst.headOption

  def toCamelCase(v: String, firstUpperCase: Boolean): String = {
    def camel(firstUpper: Boolean, v: String): String =
      if (firstUpper) v.toLowerCase.capitalize
      else v.toLowerCase
    v.split('_').toList match {
      case x :: xs => {
        val first = camel(firstUpperCase, x)
        (first :: xs.map(camel(true, _))).mkString
      }
      case _ => camel(firstUpperCase, v)
    }
  }

  def mkTypeName(v: String): String = toCamelCase(v, true)
  def mkConstName(v: String): String = mkTypeName(v) + "Type"

  def enumName(v: String): String = mkTypeName(v)
  def enumConstName(v: String): String = mkConstName(v)
  def enumMemberName(v: String): String = v.toList match {
    case x :: _ if Character.isJavaIdentifierStart(x) => v
    case _ => "V_" + v
  }

  def complexTypeName(v: String): String = mkTypeName(v)
  def complexTypeConstName(v: String): String = mkConstName(v)

  def newtypeTypeName(v: String): String = mkTypeName(v)
  def newtypeTypeConstName(v: String): String = mkConstName(v)

  def fieldName(v: String): String = toCamelCase(v, false)

  def schema(xsd: x.Schema[SrcF]): Result[ModuleDecl] =
    for {
      c <- getConfig
      _ <- setUnparsedTypes(xsd.types)
      t <- types()
    } yield
      ModuleDecl(
        ImportsDecl(c.imports).some,
        NEL.fromList(t).map(TypesDecl(_))
      )

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

  def types(): Result[List[TypeDecl]] =
    unfoldM(()) { _ =>
      popUnparsedType.map(_.traverse(`type`)).flatten.map(_.map(((), _)))
    }

  lazy val `type`: x.Type[SrcF] => Result[TypeDecl] =
    x.Type.fold(simpleType, complexType, element)

  def simpleTypeEnum(
    t: x.SimpleType[SrcF]
  )(name: String, values: List[x.Enumeration[SrcF]]): Result[TypeDecl] =
    for {
      l <- ok(
        values
          .map(e => EnumMemberDecl(Ident(enumMemberName(e.value)), e.value))
      )
      members <- NEL
        .fromList(l)
        .fold(Errors.cantTransform[NEL[EnumMemberDecl]](t))(ok(_))
    } yield
      EnumDecl(
        Ident(enumName(name)),
        Ident(enumConstName(name)),
        true,
        members
      )

  def simpleTypeNewtype(name: String, base: x.QName): Result[TypeDecl] =
    for {
      baseRef <- typeRef(base)
    } yield
      NewtypeDecl(
        Ident(newtypeTypeName(name)),
        Ident(newtypeTypeConstName(name)),
        baseRef,
        true
      )

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
          case "string" => ok("string")
          case "base64Binary" => ok("string") // TODO: Custom type!
          case "anyURI" => ok("string") // TODO: Custom type!
          case "int" => ok("number")
          case "long" => ok("number")
          case "integer" => ok("number")
          case "nonNegativeInteger" => ok("number")
          case "positiveInteger" => ok("number")
          case "decimal" => ok("number")
          case "dateTime" => ok("Date") // TODO: Custom type!
          case "anyType" => ok("any")
          case _ => Errors.unknownType[String](`type`)
        }
      } else Errors.unknownType[String](`type`)
    } yield TypeRef.std(Ident(name))

  def typeRef(`type`: x.QName): Result[TypeRef] =
    for {
      config <- getConfig
      result <- config.externalTypes
        .find(_.xsdName === `type`)
        .fold[Result[TypeRef]](`type` match {
          case x.QName(_, ns) if ns === config.xsdNs =>
            stdTypeRef(`type`)
          case x.QName(t, None) =>
            ok(
              TypeRef.defined(
                Ident(complexTypeName(t)),
                Ident(complexTypeConstName(t))
              )
            )
          case _ => Errors.unknownType[TypeRef](`type`)
        })(t => ok(TypeRef.external(t.name, t.constName)))
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
    body: x.Body[SrcF]
  ): BodyFieldHandler[Result[List[FieldDecl]]] =
    (name, `type`, minOccurs, maxOccurs, nullable) =>
      for {
        config <- getConfig
        t <- typeRef(`type`.getOrElse(x.QName("anyType", config.xsdNs)))
        result <- transformField(minOccurs, maxOccurs, nullable)(
          FieldDecl(Ident(fieldName(name)), t, FieldConstraint.Required, false)
        )
      } yield List(result)

  def bodySequence(
    b: x.Body[SrcF],
    typeName: String
  ): BodySequenceHandler[Result[List[FieldDecl]]] =
    (seq, minOccurs, maxOccurs) =>
      for {
        fields <- seq.body.extract.flatTraverse(bodyToFields(typeName))
        result <- fields.traverse(transformField(minOccurs, maxOccurs, false))
      } yield result

  def bodyChoice(
    b: x.Body[SrcF],
    typeName: String
  ): BodyChoiceHandler[Result[List[FieldDecl]]] =
    (choice, minOccurs, maxOccurs) =>
      for {
        fields <- choice.body.extract.flatTraverse(bodyToFields(typeName))
        result <- fields.traverse(transformField(minOccurs, maxOccurs, false))
      } yield result.map(_.copy(constraint = FieldConstraint.Optional))

  def bodyAnonComplexType(
    b: x.Body[SrcF],
    typeName: String
  ): BodyAnonComplexTypeHandler[Result[List[FieldDecl]]] =
    (name, ct, minOccurs, maxOccurs, nullable) => {
      val anonTypeName = typeName + "_" + name
      for {
        _ <- pushUnparsedType(
          x.Type.complexType(ct.copy[SrcF](name = anonTypeName.some))
        )
        result <- bodyField(b)(
          name,
          x.QName.fromString(anonTypeName).some,
          minOccurs,
          maxOccurs,
          nullable
        )
      } yield result
    }

  def bodyAnonSimpleType(
    b: x.Body[SrcF],
    typeName: String
  ): BodyAnonSimpleTypeHandler[Result[List[FieldDecl]]] =
    (name, st, minOccurs, maxOccurs, nullable) => {
      val anonTypeName = typeName + "_" + name
      for {
        _ <- pushUnparsedType(
          x.Type.simpleType(st.copy[SrcF](name = anonTypeName.some))
        )
        result <- bodyField(b)(
          name,
          x.QName.fromString(anonTypeName).some,
          minOccurs,
          maxOccurs,
          nullable
        )
      } yield result
    }

  def bodyToFields(
    typeName: String
  )(b: x.Body[SrcF]): Result[List[FieldDecl]] =
    for {
      folds <- getFold
      result <- folds.body(
        bodyField(b),
        bodySequence(b, typeName),
        bodyChoice(b, typeName),
        bodyAnonComplexType(b, typeName),
        bodyAnonSimpleType(b, typeName),
        Errors.cantTransform
      )(b)
    } yield result

  def attrToField(
    t: x.ComplexType[SrcF]
  )(attr: x.Attribute[SrcF]): Result[FieldDecl] =
    attr match {
      case x.Attribute(_, Some(name), _, _, Some(typ), use) =>
        for {
          t <- typeRef(x.QName.fromString(typ))
        } yield
          FieldDecl(
            Ident(fieldName(name)),
            t,
            use
              .map(_ === "optional")
              .fold[FieldConstraint](FieldConstraint.Required)(
                v =>
                  if (v) FieldConstraint.Optional else FieldConstraint.Required
              ),
            false
          )
      case _ => Errors.cantTransform(t)
    }

  def attrsToFields(
    t: x.ComplexType[SrcF]
  )(attrs: List[x.Attribute[SrcF]]): Result[List[FieldDecl]] =
    attrs.traverse(attrToField(t))

  def bodyListToType(
    t: x.ComplexType[SrcF],
    baseNameO: Option[x.QName],
    allOptional: Boolean
  )(name: String,
    b: List[x.Body[SrcF]],
    attrs: List[x.Attribute[SrcF]]): Result[TypeDecl] =
    for {
      l0 <- b.flatTraverse(bodyToFields(name))
      l <- ok(
        if (allOptional) l0.map(_.copy(constraint = FieldConstraint.Optional))
        else l0
      )
      al <- attrsToFields(t)(attrs)
      fields <- NEL
        .fromList(l ++ al)
        .fold(Errors.cantTransform[NEL[FieldDecl]](t))(ok(_))
      baseRef <- baseNameO.traverse(typeRef)
    } yield
      ComplexTypeDecl(
        Ident(complexTypeName(name)),
        Ident(complexTypeConstName(name)),
        baseRef,
        true,
        fields
      )

  def complexTypeAny(name: String, any: x.Any[SrcF]): Result[TypeDecl] =
    for {
      config <- getConfig
      `type` <- typeRef(x.QName("anyType", config.xsdNs))
    } yield
      NewtypeDecl(
        Ident(newtypeTypeName(name)),
        Ident(newtypeTypeConstName(name)),
        `type`,
        true
      )

  def complexType(t: x.ComplexType[SrcF]): Result[TypeDecl] =
    getFold flatMap { fold =>
      fold
        .complexType[Result[TypeDecl]](
          (name, bName, seq, attrs) =>
            bodyListToType(t, bName, false)(name, seq.body, attrs),
          (name, ch, attrs) =>
            bodyListToType(t, None, true)(name, ch.body, attrs),
          complexTypeAny,
          Errors.cantTransform
        )(t)
    }

  def element(t: x.Element[SrcF]): Result[TypeDecl] =
    getFold flatMap {
      _.element[Result[TypeDecl]](
        (name, ct) => complexType(ct.copy[SrcF](name = Some(name))),
        Errors.cantTransform
      )(t)
    }
}
