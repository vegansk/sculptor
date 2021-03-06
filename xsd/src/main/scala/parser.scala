package sculptor.xsd

import sculptor.xsd.{fold => f, ast => a}
import cats._, cats.implicits._
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

      val appinfoOp = f.appinfoOp[a.Annotation[F]] { node => ann =>
        f.ok(
          ann.copy(
            appinfo =
              combineAlternative(ann.appinfo, pure(Some(List(node.text))))
          )
        )
      }

      def fromSetter[A[_[_]]](setter: Setter[A[F], a.Annotation[F]]) =
        f.annotationOp[A[F]] { node => ast =>
          for {
            ann <- f.annotation(
              appinfo = appinfoOp,
              documentation = documentationOp
            )(node)(a.Annotation.empty[F])
          } yield setter(ann)(ast)
        }
    }

    object enumeration {
      def fromSetter[A[_[_]]](setter: Setter[A[F], a.Enumeration[F]]) =
        f.enumerationOp[A[F]] { node => ast =>
          for {
            e <- f.enumeration(value = valueOp, annotation = annotationOp)(
              node
            )(a.Enumeration.empty[F])
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

      def fromSetter[A[_[_]]](
        setter: Setter[A[F], a.SimpleTypeRestriction[F]]
      ) = f.simpleTypeRestrictionOp[A[F]] { node => ast =>
        for {
          r <- f.simpleTypeRestriction(
            base = baseOp,
            whiteSpace = f.whiteSpaceOp(f.nop),
            pattern = patternOp,
            minLength = minLengthOp,
            maxLength = maxLengthOp,
            fractionDigits = fractionDigitsOp,
            totalDigits = totalDigitsOp,
            minInclusive = minInclusiveOp,
            enumeration = enumerationOp
          )(node)(a.SimpleTypeRestriction.empty[F])
        } yield setter(r)(ast)
      }

      def baseOp = f.baseOp[a.SimpleTypeRestriction[F]] { base => r =>
        f.ok(r.copy(base = pure(a.QName.fromString(base))))
      }

      def patternOp = f.pattern[a.SimpleTypeRestriction[F]] {
        f.valueOp { pattern => r =>
          f.ok(r.copy(pattern = pure(Some(pattern))))
        }
      }

      def minLengthOp = f.minLength[a.SimpleTypeRestriction[F]] {
        f.valueOp { v => ast =>
          f.ok(ast.copy(minLength = pure(Some(v))))
        }
      }

      def maxLengthOp = f.maxLength[a.SimpleTypeRestriction[F]] {
        f.valueOp { v => ast =>
          f.ok(ast.copy(maxLength = pure(Some(v))))
        }
      }

      def fractionDigitsOp = f.fractionDigits[a.SimpleTypeRestriction[F]] {
        f.valueOp { v => ast =>
          f.ok(ast.copy(fractionDigits = pure(Some(v))))
        }
      }

      def totalDigitsOp = f.totalDigits[a.SimpleTypeRestriction[F]] {
        f.valueOp { v => ast =>
          f.ok(ast.copy(totalDigits = pure(Some(v))))
        }
      }

      def minInclusiveOp = f.minInclusive[a.SimpleTypeRestriction[F]] {
        f.valueOp { v => ast =>
          f.ok(ast.copy(minInclusive = pure(Some(v))))
        }
      }

      def enumerationOp = enumeration.fromSetter[a.SimpleTypeRestriction] {
        e => r =>
          r.copy(enumeration = combineAlternative(r.enumeration, pure(List(e))))
      }
    }

    object simpleType {
      def fromSetter[A[_[_]]](setter: Setter[A[F], a.SimpleType[F]]) =
        f.simpleTypeOp[A[F]] { node => ast =>
          for {
            st <- f.simpleType(
              annotation = annotationOp,
              name = nameOp,
              `final` = finalOp,
              restriction = restrictionOp
            )(node)(a.SimpleType.empty[F])
          } yield setter(st)(ast)
        }

      def nameOp = f.nameOp[a.SimpleType[F]] { name => st =>
        f.ok(st.copy(name = pure(Some(name))))
      }

      def finalOp = f.finalOp[a.SimpleType[F]] { v => ast =>
        f.ok(ast.copy(`final` = pure(Some(v))))
      }

      def annotationOp = annotation.fromSetter[a.SimpleType] { ann => st =>
        st.copy(annotation = pure(Some(ann)))
      }

      def restrictionOp = simpleTypeRestriction.fromSetter[a.SimpleType] {
        r => st =>
          st.copy(restriction = pure(Some(r)))
      }

    }

    object attribute {
      def fromSetter[A[_[_]]](setter: Setter[A[F], a.Attribute[F]]) =
        f.attributeOp[A[F]] { node => ast =>
          for {
            v <- f.attribute(
              annotation = annotationOp,
              name = nameOp,
              form = formOp,
              fixed = fixedOp,
              `type` = typeOp,
              use = useOp
            )(node)(a.Attribute.empty[F])
          } yield setter(v)(ast)
        }

      def nameOp = f.nameOp[a.Attribute[F]] { v => ast =>
        f.ok(ast.copy(name = pure(Some(v))))
      }

      def formOp = f.formOp[a.Attribute[F]] { v => ast =>
        f.ok(ast.copy(form = pure(Some(v))))
      }

      def useOp = f.useOp[a.Attribute[F]] { v => ast =>
        f.ok(ast.copy(use = pure(Some(v))))
      }

      def fixedOp = f.fixedOp[a.Attribute[F]] { v => ast =>
        f.ok(ast.copy(fixed = pure(Some(v))))
      }

      def typeOp = f.typeOp[a.Attribute[F]] { v => ast =>
        f.ok(ast.copy(`type` = pure(Some(v))))
      }

      def annotationOp = annotation.fromSetter[a.Attribute] { v => ast =>
        ast.copy(annotation = pure(Some(v)))
      }
    }

    object any {
      def fromSetter[A[_[_]]](setter: Setter[A[F], a.Any[F]]) =
        f.anyOp[A[F]] { node => ast =>
          for {
            v <- f.any(
              annotation = annotationOp,
              namespace = namespaceOp,
              processContents = processContentsOp,
              minOccurs = minOccursOp,
              maxOccurs = maxOccursOp
            )(node)(a.Any.empty[F])
          } yield setter(v)(ast)
        }

      def minOccursOp = f.minOccursOp[a.Any[F]] { v => ast =>
        f.ok(ast.copy(minOccurs = pure(Some(v))))
      }

      def maxOccursOp = f.maxOccursOp[a.Any[F]] { v => ast =>
        f.ok(ast.copy(maxOccurs = pure(Some(v))))
      }

      def namespaceOp = f.namespaceOp[a.Any[F]] { v => ast =>
        f.ok(ast.copy(namespace = pure(Some(v))))
      }

      def processContentsOp = f.processContentsOp[a.Any[F]] { v => ast =>
        f.ok(ast.copy(processContents = pure(Some(v))))
      }

      def annotationOp = annotation.fromSetter[a.Any] { v => ast =>
        ast.copy(annotation = pure(Some(v)))
      }
    }

    object choice {
      def fromSetter[A[_[_]]](setter: Setter[A[F], a.Choice[F]]) =
        f.choiceOp[A[F]] { node => ast =>
          for {
            ch <- f.choice(
              choice = choiceOp,
              element = elementOp,
              minOccurs = minOccursOp,
              maxOccurs = maxOccursOp,
              sequence = sequenceOp,
              annotation = annotationOp
            )(node)(a.Choice.empty[F])
          } yield setter(ch)(ast)
        }

      def choiceOp: f.ChoiceOp[a.Choice[F]] = choice.fromSetter { v => ast =>
        ast.copy(
          body = combineAlternative(ast.body, pure(List(a.Body.choice[F](v))))
        )
      }

      def elementOp: f.ElementOp[a.Choice[F]] = element.fromSetter { v => ast =>
        ast.copy(
          body = combineAlternative(ast.body, pure(List(a.Body.element[F](v))))
        )
      }

      def sequenceOp: f.SequenceOp[a.Choice[F]] = sequence.fromSetter {
        v => ast =>
          ast.copy(
            body =
              combineAlternative(ast.body, pure(List(a.Body.sequence[F](v))))
          )
      }

      def minOccursOp = f.minOccursOp[a.Choice[F]] { minOccurs => ast =>
        f.ok(ast.copy(minOccurs = pure(Some(minOccurs))))
      }

      def maxOccursOp = f.maxOccursOp[a.Choice[F]] { v => ast =>
        f.ok(ast.copy(maxOccurs = pure(Some(v))))
      }

      def annotationOp = annotation.fromSetter[a.Choice] { v => ast =>
        ast.copy(annotation = pure(Some(v)))
      }
    }

    object sequence {
      def fromSetter[A[_[_]]](setter: Setter[A[F], a.Sequence[F]]) =
        f.sequenceOp[A[F]] { node => ast =>
          for {
            seq <- f.sequence(
              element = elementOp,
              choice = choiceOp,
              sequence = sequenceOp,
              minOccurs = minOccursOp,
              maxOccurs = maxOccursOp,
              annotation = annotationOp,
              any = anyOp
            )(node)(a.Sequence.empty[F])
          } yield setter(seq)(ast)
        }

      def choiceOp = choice.fromSetter[a.Sequence] { v => ast =>
        ast.copy(
          body = combineAlternative(ast.body, pure(List(a.Body.choice[F](v))))
        )
      }

      def anyOp = any.fromSetter[a.Sequence] { v => ast =>
        ast.copy(
          body = combineAlternative(ast.body, pure(List(a.Body.any[F](v))))
        )
      }

      def sequenceOp: f.SequenceOp[a.Sequence[F]] = fromSetter { v => ast =>
        ast.copy(
          body = combineAlternative(ast.body, pure(List(a.Body.sequence[F](v))))
        )
      }

      def elementOp: f.ElementOp[a.Sequence[F]] = element.fromSetter {
        v => ast =>
          ast.copy(
            body =
              combineAlternative(ast.body, pure(List(a.Body.element[F](v))))
          )
      }

      def minOccursOp = f.minOccursOp[a.Sequence[F]] { minOccurs => ast =>
        f.ok(ast.copy(minOccurs = pure(Some(minOccurs))))
      }

      def maxOccursOp = f.maxOccursOp[a.Sequence[F]] { v => ast =>
        f.ok(ast.copy(maxOccurs = pure(Some(v))))
      }

      def annotationOp = annotation.fromSetter[a.Sequence] { v => ast =>
        ast.copy(annotation = pure(Some(v)))
      }
    }

    object complexContentExtension {
      def fromSetter[A[_[_]]](
        setter: Setter[A[F], a.ComplexContentExtension[F]]
      ) = f.complexContentExtensionOp[A[F]] { node => ast =>
        for {
          el <- f.complexContentExtension(
            annotation = annotationOp,
            base = baseOp,
            sequence = sequenceOp
          )(node)(a.ComplexContentExtension.empty[F])
        } yield setter(el)(ast)
      }

      def annotationOp = annotation.fromSetter[a.ComplexContentExtension] {
        ann => ast =>
          ast.copy(annotation = pure(Some(ann)))
      }

      def baseOp = f.baseOp[a.ComplexContentExtension[F]] { base => ast =>
        f.ok(ast.copy(base = pure(a.QName.fromString(base))))
      }

      def sequenceOp: f.SequenceOp[a.ComplexContentExtension[F]] =
        sequence.fromSetter { seq => ast =>
          ast.copy(sequence = pure(Some(seq)))
        }
    }

    object complexContent {
      def fromSetter[A[_[_]]](setter: Setter[A[F], a.ComplexContent[F]]) =
        f.complexContentOp[A[F]] { node => ast =>
          for {
            el <- f.complexContent(
              annotation = annotationOp,
              extension = extensionOp
            )(node)(a.ComplexContent.empty[F])
          } yield setter(el)(ast)
        }

      def annotationOp = annotation.fromSetter[a.ComplexContent] { ann => ast =>
        ast.copy(annotation = pure(Some(ann)))
      }

      def extensionOp = complexContentExtension.fromSetter[a.ComplexContent] {
        ext => ast =>
          ast.copy(extension = pure(Some(ext)))
      }
    }

    object simpleContentExtension {
      def fromSetter[A[_[_]]](
        setter: Setter[A[F], a.SimpleContentExtension[F]]
      ) =
        f.simpleContentExtensionOp[A[F]] { node => ast =>
          for {
            el <- f.simpleContentExtension(
              annotation = annotationOp,
              base = baseOp,
              attribute = attributeOp
            )(node)(a.SimpleContentExtension.empty[F])
          } yield setter(el)(ast)
        }

      def annotationOp = annotation.fromSetter[a.SimpleContentExtension] {
        ann => ast =>
          ast.copy(annotation = pure(Some(ann)))
      }

      def baseOp = f.baseOp[a.SimpleContentExtension[F]] { base => r =>
        f.ok(r.copy(base = pure(a.QName.fromString(base))))
      }

      def attributeOp = attribute.fromSetter[a.SimpleContentExtension] {
        v => ast =>
          ast.copy(
            attributes = combineAlternative(ast.attributes, pure(List(v)))
          )
      }
    }

    object simpleContent {
      def fromSetter[A[_[_]]](setter: Setter[A[F], a.SimpleContent[F]]) =
        f.simpleContentOp[A[F]] { node => ast =>
          for {
            el <- f.simpleContent(
              annotation = annotationOp,
              extension = extensionOp
            )(node)(a.SimpleContent.empty[F])
          } yield setter(el)(ast)
        }

      def annotationOp = annotation.fromSetter[a.SimpleContent] { ann => ast =>
        ast.copy(annotation = pure(Some(ann)))
      }

      def extensionOp = simpleContentExtension.fromSetter[a.SimpleContent] {
        e => ast =>
          ast.copy(extension = pure(Some(e)))
      }
    }

    object complexType {
      def fromSetter[A[_[_]]](setter: Setter[A[F], a.ComplexType[F]]) =
        f.complexTypeOp[A[F]] { node => ast =>
          for {
            el <- f.complexType(
              name = nameOp,
              annotation = annotationOp,
              complexContent = complexContentOp,
              simpleContent = simpleContentOp,
              attribute = attributeOp,
              sequence = sequenceOp,
              choice = choiceOp,
              mixed = mixedOp,
              block = blockOp,
              `abstract` = abstractOp,
              `final` = finalOp
            )(node)(a.ComplexType.empty[F])
          } yield setter(el)(ast)
        }

      def nameOp = f.nameOp[a.ComplexType[F]] { name => ast =>
        f.ok(ast.copy(name = pure(Some(name))))
      }

      def mixedOp = f.mixedOp[a.ComplexType[F]] { v => ast =>
        f.ok(ast.copy(mixed = pure(Some(v))))
      }

      def finalOp = f.finalOp[a.ComplexType[F]] { v => ast =>
        f.ok(ast.copy(`final` = pure(Some(v))))
      }

      def blockOp = f.blockOp[a.ComplexType[F]] { v => ast =>
        f.ok(ast.copy(block = pure(Some(v))))
      }

      def abstractOp = f.abstractOp[a.ComplexType[F]] { v => ast =>
        f.ok(ast.copy(`abstract` = pure(Some(v))))
      }

      def annotationOp = annotation.fromSetter[a.ComplexType] { ann => ast =>
        ast.copy(annotation = pure(Some(ann)))
      }

      def complexContentOp = complexContent.fromSetter[a.ComplexType] {
        cc => ast =>
          ast.copy(complexContent = pure(Some(cc)))
      }

      def simpleContentOp = simpleContent.fromSetter[a.ComplexType] {
        sc => ast =>
          ast.copy(simpleContent = pure(Some(sc)))
      }

      def sequenceOp: f.SequenceOp[a.ComplexType[F]] = sequence.fromSetter {
        seq => ast =>
          ast.copy(sequence = pure(Some(seq)))
      }

      def choiceOp: f.ChoiceOp[a.ComplexType[F]] = choice.fromSetter {
        v => ast =>
          ast.copy(choice = pure(Some(v)))
      }

      def attributeOp = attribute.fromSetter[a.ComplexType] { v => ast =>
        ast.copy(attributes = combineAlternative(ast.attributes, pure(List(v))))
      }
    }

    object element {
      def fromSetter[A[_[_]]](setter: Setter[A[F], a.Element[F]]) =
        f.elementOp[A[F]] { node => ast =>
          for {
            el <- f.element(
              name = nameOp,
              annotation = annotationOp,
              complexType = complexTypeOp,
              simpleType = simpleTypeOp,
              `type` = typeOp,
              minOccurs = minOccursOp,
              maxOccurs = maxOccursOp,
              nillable = nillableOp
            )(node)(a.Element.empty[F])
          } yield setter(el)(ast)
        }

      def nameOp = f.nameOp[a.Element[F]] { name => ast =>
        f.ok(ast.copy(name = pure(Some(name))))
      }

      def typeOp = f.typeOp[a.Element[F]] { `type` => ast =>
        f.ok(ast.copy(`type` = pure(Some(a.QName.fromString(`type`)))))
      }

      def minOccursOp = f.minOccursOp[a.Element[F]] { minOccurs => ast =>
        f.ok(ast.copy(minOccurs = pure(Some(minOccurs))))
      }

      def maxOccursOp = f.maxOccursOp[a.Element[F]] { maxOccurs => ast =>
        f.ok(ast.copy(maxOccurs = pure(Some(maxOccurs))))
      }

      def nillableOp = f.nillableOp[a.Element[F]] { v => ast =>
        f.ok(ast.copy(nillable = pure(Some(v))))
      }

      def annotationOp = annotation.fromSetter[a.Element] { ann => ast =>
        ast.copy(annotation = pure(Some(ann)))
      }

      def simpleTypeOp = simpleType.fromSetter[a.Element] { v => ast =>
        ast.copy(simpleType = pure(Some(v)))
      }

      def complexTypeOp = complexType.fromSetter[a.Element] { ct => ast =>
        ast.copy(complexType = pure(Some(ct)))
      }
    }

    object schema {
      def simpleTypeOp = simpleType.fromSetter[a.Schema] { st => s =>
        s.copy(
          types =
            combineAlternative(s.types, pure(List(a.Type.simpleType[F](st))))
        )
      }

      def complexTypeOp = complexType.fromSetter[a.Schema] { ct => ast =>
        ast.copy(
          types =
            combineAlternative(ast.types, pure(List(a.Type.complexType[F](ct))))
        )
      }

      def elementOp = element.fromSetter[a.Schema] { el => s =>
        s.copy(
          types = combineAlternative(s.types, pure(List(a.Type.element[F](el))))
        )
      }

      def annotationOp = annotation.fromSetter[a.Schema] { ann => s =>
        s.copy(annotation = pure(Some(ann)))
      }
    }

    def parse(n: Node): f.Result[a.Schema[F]] =
      f.schema[a.Schema[F]](
        annotation = schema.annotationOp,
        simpleType = schema.simpleTypeOp,
        complexType = schema.complexTypeOp,
        element = schema.elementOp
      )(n)(a.Schema.empty[F])

    def run(n: Node): Either[String, a.Schema[F]] =
      parse(n).value.run(f.FoldState()).value._2
  }

}
