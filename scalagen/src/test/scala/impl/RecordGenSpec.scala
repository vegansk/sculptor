package sculptor.scalagen
package impl

import cats.implicits._
import org.specs2._

object RecordGenSpec
    extends mutable.Specification
    with ScalaCheck
    with testing.Helpers {

  import sculptor.ast._
  import dsl._

  val cfg = Config(generateComments = false)

  "RecordGen" should {

    "generate simple records" >> {
      val r = record("Record")
        .field("id", "Int".spec)
        .field("nameO", "Option".spec("String".spec))
        .build
      runGen(RecordGen.generate(r), cfg) must beEqvTo(
        "final case class Record(id: Int, nameO: Option[String])".fix.asRight
      )
    }

    val rec = record("Record")
      .comment("The Record")
      .generic("A".gen)
      .field("id", "Int".spec, "The id")
      .field("nameO", "Option".spec("A".gen), "The name")
      .additionalCodeS("/* Additional comment */")
      .build

    "generate generic records" >> {

      runGen(RecordGen.generate(rec), cfg) must beEqvTo(
        "final case class Record[A](id: Int, nameO: Option[A])".fix.asRight
      )
    }

    "generate Eq typeclass" >> {
      runGen(
        RecordGen.generate(rec),
        cfg.copy(features = List(Feature.CatsEqTypeclass))
      ) must beEqvTo(
        """|final case class Record[A](id: Int, nameO: Option[A])
           |
           |object Record {implicit def RecordEq[A]: Eq[Record[A]] = Eq.fromUniversalEquals}""".fix.asRight
      )
    }

    "generate circe codecs" >> {
      runGen(
        RecordGen.generate(rec),
        cfg.copy(features = List(Feature.CirceCodecs()))
      ) must beEqvTo(
        """|final case class Record[A](id: Int, nameO: Option[A])
           |
           |object Record {
           |  implicit def RecordEncoder[A:Encoder]: Encoder.AsObject[Record[A]] = Encoder.AsObject.instance[Record[A]] { v =>
           |    JsonObject(
           |      "id" := v.id,
           |      "nameO" := v.nameO
           |    )
           |  }
           |
           |  implicit def RecordDecoder[A:Decoder]: Decoder[Record[A]] = Decoder.instance[Record[A]] { c =>
           |    for {
           |      id <- c.downField("id").as[Int]
           |      nameO <- c.downField("nameO").as[Option[A]]
           |    } yield Record[A](id, nameO)
           |  }
           |}""".fix.asRight
      )
    }

    "generate tapir Schema and Validator" >> {
      runGen(
        RecordGen.generate(rec),
        cfg.copy(features = List(Feature.TapirSchema()))
      ) must beEqvTo(
        """|final case class Record[A](id: Int, nameO: Option[A])
           |
           |object Record {
           |  implicit def RecordSchema[A:Schema]: Schema[Record[A]] =
           |    Schema.derive[Record[A]]
           |      .description("The Record")
           |      .modify(_.id)(_.description("The id"))
           |      .modify(_.nameO)(_.description("The name"))
           |
           |  implicit def RecordValidator[A:Validator]: Validator[Record[A]] =
           |    Validator.derive[Record[A]]
           |}""".fix.asRight
      )
    }

    "generate comments" >> {
      runGen(RecordGen.generate(rec), cfg.copy(generateComments = true)) must beEqvTo(
        """|// Record Record[A]:
           |// The Record
           |
           |final case class Record[A](id: Int /* The id */, nameO: Option[A] /* The name */)""".fix.asRight
      )
    }

    "generate additional code" >> {

      runGen(
        RecordGen.generate(rec),
        cfg.copy(features = Feature.AdditionalCode.pure[List])
      ) must beEqvTo("""|final case class Record[A](id: Int, nameO: Option[A])
           |
           |object Record {/* Additional comment */}""".fix.asRight)
    }
  }

}
