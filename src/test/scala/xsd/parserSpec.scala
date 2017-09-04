package sculptor.xsd

import org.specs2._
import sculptor._
import types._
import scala.xml.XML

object parserSpec extends mutable.Specification {

  "xsd parser" should {

    "parse valid xsd" >> {

      val xsd =
        getClass.getClassLoader.getResourceAsStream("xsd/parserSpec_01.xsd")

      parser(XML.load(xsd)) must_== Right(
        ModuleF(
          None,
          Map(
            Ident("rstr_t") ->
              TypeT(
                RestrictedStringF(
                  Ident("rstr_t"),
                  TypeT(StringF()),
                  Some(1),
                  Some(10),
                  List("[a-z]+", "[abc]+")
                )
              ),
            Ident("rint_t") ->
              TypeT(
                RestrictedNumberF(Ident("rint_t"), TypeT(IntF()))
              ),
            Ident("rec_t") ->
              TypeT(
                RecordF(
                  Ident("rec_t"),
                  List(Ident("str") -> TypeT(StringF()), Ident("int") -> TypeT(IntF()))
                )
              )
          )
        )
      )

    }

    "link known types" >> {
      val xsd =
        getClass.getClassLoader.getResourceAsStream("xsd/parserSpec_02.xsd")

      parser(XML.load(xsd)) must_== Right(
        ModuleF(
          None,
          Map(
            Ident("rstr_t") ->
              TypeT(
                RestrictedStringF(Ident("rstr_t"), TypeT(StringF())
                )
              ),
            Ident("rint_t") ->
              TypeT(
                RestrictedNumberF(Ident("rint_t"), TypeT(IntF()))
              ),
            Ident("rec_t") ->
              TypeT(
                RecordF(
                  Ident("rec_t"),
                  List(Ident("str") -> TypeT(TypeIdF(Ident("rstr_t"))), Ident("int") -> TypeT(TypeIdF(Ident("rint_t"))))
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
      println(err)
      err.isLeft must_== true
    }
  }

}
