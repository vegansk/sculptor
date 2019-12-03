package sbttest

import test.types._
import io.circe._
import io.circe.syntax._
import io.circe.parser.decode

object Main extends App {

  println(MyInt(1).asJson)
  println(decode[MyInt]("1"))

  println(Record(1, "test").asJson)
  println(decode[Record]("""{ "id": 1, "name": "test" }"""))

  println((Maybe.just(1)).asJson)
  println(decode[Maybe[Int]]("""{ "__tag": "Just", "get": 1 }"""))

  println(Maybe.fold[Int, String]("nothing", _.toString)(Maybe.just(1)))

  println((Enum.A:Enum).asJson)
  println(decode[Enum]("\"A\""))

}
