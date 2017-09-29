package sculptor.xsd

import sculptor.xsd.{fold => f, ast => a}
import cats._, cats.implicits._, cats.instances._
import scala.xml._

object parser {

  def apply[F[_]: Alternative]: impl[F] = new impl[F]

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  class impl[F[_]: Alternative] {

    type Setter[S, A] = A => S => S

    private def pure[A](v: A) = Applicative[F].pure(v)

    private def combineAlternative[A: Monoid](x: F[A], y: F[A]): F[A] = {
      val zeroF = Monoid[A].empty.pure[F]
      Alternative[F].map2(x.combineK(zeroF), y.combineK(zeroF))(
        Monoid[A].combine
      )
    }

    object annotation {

      val documentationOp = f.documentationOp[a.Annotation[F]] { node => ann =>
        f.ok(
          ann.copy(
            documentation =
              combineAlternative(ann.documentation, pure(List(node.text)))
          )
        )
      }

      def fromSetter[A[?[_]] <: a.AST[?]](
        setter: Setter[A[F], a.Annotation[F]]
      ) =
        f.annotationOp[A[F]] { node => ast =>
          for {
            ann <- f.annotation(documentation = documentationOp)(node)(
              a.Annotation.empty[F]
            )
          } yield setter(ann)(ast)
        }
    }

    object enumeration {
      def fromSetter[A[?[_]] <: a.AST[?]](
        setter: Setter[A[F], a.Enumeration[F]]
      ) = f.enumerationOp[A[F]] { node => ast =>
        for {
          e <- f.enumeration(value = valueOp, annotation = annotationOp)(node)(
            a.Enumeration.empty[F]
          )
        } yield setter(e)(ast)
      }

      def valueOp = f.valueOp[a.Enumeration[F]] { value => e =>
        f.ok(e.copy(value = pure(value)))
      }

      def annotationOp = annotation.fromSetter[a.Enumeration] { ann => e =>
        e.copy(annotation = pure(Some(ann)))
      }
    }

    object simpleTypeRestriction {

      def fromSetter[A[?[_]] <: a.AST[?]](
        setter: Setter[A[F], a.SimpleTypeRestriction[F]]
      ) = f.simpleTypeRestrictionOp[A[F]] { node => ast =>
        for {
          r <- f.simpleTypeRestriction(
            base = baseOp,
            whiteSpace = f.whiteSpaceOp(f.nop),
            pattern = patternOp,
            enumeration = enumerationOp
          )(node)(a.SimpleTypeRestriction.empty[F])
        } yield setter(r)(ast)
      }

      def baseOp = f.baseOp[a.SimpleTypeRestriction[F]] { base => r =>
        f.ok(r.copy(base = pure(base)))
      }

      def patternOp = f.pattern[a.SimpleTypeRestriction[F]] {
        f.valueOp { pattern => r =>
          f.ok(r.copy(pattern = pure(Some(pattern))))
        }
      }

      def enumerationOp = enumeration.fromSetter[a.SimpleTypeRestriction] {
        e => r =>
          r.copy(
            enumeration = combineAlternative(r.enumeration, pure(List(e)))
          )
      }

    }

    object simpleType {
      def fromSetter[A[?[_]] <: a.AST[?]](
        setter: Setter[A[F], a.SimpleType[F]]
      ) =
        f.simpleTypeOp[A[F]] { node => ast =>
          for {
            st <- f.simpleType(
              annotation = annotationOp,
              name = nameOp,
              restriction = restrictionOp
            )(node)(a.SimpleType.empty[F])
          } yield setter(st)(ast)
        }

      def nameOp = f.nameOp[a.SimpleType[F]] { name => st =>
        f.ok(st.copy(name = pure(Some(name))))
      }

      def annotationOp = annotation.fromSetter[a.SimpleType] { ann => st =>
        st.copy(annotation = pure(Some(ann)))
      }

      def restrictionOp = simpleTypeRestriction.fromSetter[a.SimpleType] {
        r => st =>
          st.copy(restriction = pure(Some(r)))
      }

    }

    object schema {
      def simpleTypeOp = simpleType.fromSetter[a.Schema] { st => s =>
        s.copy(
          types =
            combineAlternative(s.types, pure(List(a.Type(a.Type.Simple(st)))))
        )
      }

      def annotationOp = annotation.fromSetter[a.Schema] { ann => s =>
        s.copy(annotation = pure(Some(ann)))
      }
    }

    def parse(n: Node): f.Result[a.Schema[F]] =
      f.schema[a.Schema[F]](
        annotation = schema.annotationOp,
        simpleType = schema.simpleTypeOp
      )(n)(a.Schema.empty[F])

  }

}
