import sculptor.ast.dsl._

object Simple {

  val nt = newtype("MyInt").baseType("Int".spec)

  // See https://gitlab.com/vegansk/sculptor/issues/97
  // val a = alias("Id").generic("A".gen).baseType("A".gen)

  val a = adt("Maybe").generic("A".gen)
    .constructors(
      cons("Nothing").generic("A".gen),
      cons("Just").generic("A".gen).field("get", "A".gen)
    )
    .additionalCodeS(
      """|def fold[A, B](empty: => B, f: A => B)(fa: Maybe[A]): B = {
         |  fa match {
         |    case Nothing() => empty
         |    case Just(a) => f(a)
         |  }
         |}""".stripMargin)

  val r = record("Record")
    .field("id", "Int".spec)
    .field("name", "String".spec)

  val e = enum("Enum").values(
    "A".en, "B".en, "C".en
  )

  val packageAst = pkg("test.types")
    .types(
      nt, r, a, e
    )
    .build

}
