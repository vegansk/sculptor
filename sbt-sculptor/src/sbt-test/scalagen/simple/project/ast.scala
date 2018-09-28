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

  val r = record("Record")
    .field("id", "Int".spec)
    .field("name", "String".spec)

  val packageAst = pkg("test.types")
    .types(
      nt, r, a
    )
    .build

}
