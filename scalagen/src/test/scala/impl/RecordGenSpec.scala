package sculptor.scalagen
package impl

import cats.implicits._
import org.specs2._
import org.typelevel.paiges._

object RecordGenSpec extends mutable.Specification
    with ScalaCheck
    with testing.CatsEqMatcher {

  import testing.paiges._
  import sculptor.ast._
  import dsl._

  val cfg = Config()

  "RecordGen" should {

    "generate simple records" >> {

      val r = record("Record")
        .field("id", "Int".spec)
        .field("nameO", "Option".spec("String".spec))
        .build

      run(RecordGen.generate(r), cfg) must beEqvTo(
        Doc.text("final case class Record(id: Int, nameO: Option[String])").asRight
      )
    }

    "generate generic records" >> {

      val r = record("Record").generic("A".gen)
        .field("id", "Int".spec)
        .field("nameO", "Option".spec("A".gen))
        .build

      run(RecordGen.generate(r), cfg) must beEqvTo(
        Doc.text("final case class Record[A](id: Int, nameO: Option[A])").asRight
      )
    }

    "generate Eq typeclass" >> {
      val r = record("Record").generic("A".gen)
        .field("id", "Int".spec)
        .field("nameO", "Option".spec("A".gen))
        .build

      run(RecordGen.generate(r), cfg.copy(features = List(Feature.CatsEqTypeclass))) must beEqvTo(
        Doc.text("""|final case class Record[A](id: Int, nameO: Option[A])
                    |
                    |object Record {
                    |  implicit def RecordEq[A]: Eq[Record[A]] = Eq.fromUniversalEquals
                    |}""".stripMargin).asRight
      )
    }

    "generate circe codecs" >> {
      val r = record("Record").generic("A".gen)
        .field("id", "Int".spec)
        .field("nameO", "Option".spec("A".gen))
        .build

      run(RecordGen.generate(r).map(_.render(cfg.lineWidth)), cfg.copy(features = List(Feature.CirceCodecs()))) must beEqvTo(
        """|final case class Record[A](id: Int, nameO: Option[A])
           |
           |object Record {
           |  implicit def RecordEncoder[A:Encoder]: ObjectEncoder[Record[A]] = ObjectEncoder.instance[Record[A]] { v =>
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
           |}""".stripMargin.asRight
      )
    }
  }

}
