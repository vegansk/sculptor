package sbttest

import test.types._

object Main extends App {

  println(MyInt(1))

  println(Record(1, "test"))

  println(Maybe.Just(1))

  println(Enum.A)

}
