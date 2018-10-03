import sculptor.ast.dsl._

object Simple {

  val nt = newtype("MyInt").baseType("number".spec)

  val al = alias("Id").generic("A".gen).baseType("A".gen)

  val a = adt("Maybe").generic("A".gen)
    .constructors(
      cons("Nothing").generic("A".gen),
      cons("Just").generic("A".gen).field("get", "A".gen)
    )

  val r = record("Record")
    .field("id", "number".spec)
    .field("name", "string".spec)

  val e = enum("Enum").values(
    "A".en, "B".en, "C".en
  )

  val packageAst = pkg("test.types")
    .types(
      nt, al, r, a, e
    )
    .build

}
