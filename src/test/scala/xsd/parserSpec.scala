package sculptor.xsd

import org.specs2._
import sculptor._
import types._
import scala.xml.XML

object parserSpec extends mutable.Specification {

  def field(name: String, `type`: TypeF[TypeT]): TypeT = TypeT(FieldF(Ident(name), TypeT(`type`)))

  val rstr_t = NamedTypeF(
    Ident("rstr_t"),
    RestrictedStringF(
      TypeT(StringF()),
      Some(1),
      Some(10),
      List("[a-z]+", "[abc]+")
    )
  )

  val rint_t = NamedTypeF(Ident("rint_t"), RestrictedNumberF(TypeT(IntegerF())))

  val rec_t = NamedTypeF(
    Ident("rec_t"),
    ComplexTypeF(
      Sequence(
        List(
          field("str", StringF()),
          field("int", IntegerF()),
          field("anonDecimal", RestrictedNumberF(TypeT(DecimalF()), minExclusive = Some(0))),
          field("subRec",
                ComplexTypeF(Sequence(List(field("subEl", StringF()))))
          ),
          TypeT(
            ComplexTypeF(Sequence(List(field("seqStr", StringF()), field("seqInt", IntegerF()))))
          ),
          TypeT(
            ComplexTypeF(Choice(List(field("seqStr", StringF()), field("seqInt", IntegerF()))))
          )
        )
      )
    )
  )

  val rec2_t = NamedTypeF(
    Ident("rec2_t"),
    ComplexTypeF(
      All(
        List(field("str", StringF()), field("int", IntegerF()))
      )
    )
  )

  "xsd parser" should {

    "parse valid xsd" >> {

      val xsd =
        getClass.getClassLoader.getResourceAsStream("xsd/parserSpec_01.xsd")

      val module = parser(XML.load(xsd))
      module.isRight must_== true
      val types = module.map(_.types).getOrElse(Map())
      types(rstr_t.name) must_== TypeT(rstr_t)
      types(rint_t.name) must_== TypeT(rint_t)
      types(rec_t.name) must_== TypeT(rec_t)
      types(rec2_t.name) must_== TypeT(rec2_t)

    }

    "link known types" >> {
      val xsd =
        getClass.getClassLoader.getResourceAsStream("xsd/parserSpec_02.xsd")

      parser(XML.load(xsd)) must_== Right(
        ModuleF(
          None,
          Map(
            rstr_t.name -> TypeT(rstr_t),
            rint_t.name -> TypeT(rint_t),
            Ident("rec_t") ->
              TypeT(
                NamedTypeF(
                  Ident("rec_t"),
                  ComplexTypeF(
                    Sequence(List[TypeT](field("str", TypeIdF(rstr_t.name)), field("int", TypeIdF(rint_t.name))))
                  )
                )
              )
          )
        )
      )

    }

    "issue an error about unknown types" >> {
      val xsd =
        getClass.getClassLoader.getResourceAsStream("xsd/parserSpec_03.xsd")

      val err = parser(XML.load(xsd))
      err.isLeft must_== true
    }

    "be able to parse fes-1.0 schema" >> {
      val xsd =
        getClass.getClassLoader.getResourceAsStream("xsd/fes-1.0.xsd")

      val err = parser(XML.load(xsd))
      println(err)
      err.isLeft must_== true
    }
  }

}
