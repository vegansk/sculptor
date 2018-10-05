import sculptor.ast.dsl._

object Simple {

  val MyInt = newtype("MyInt").baseType("number".spec)

  val Id = alias("Id").generic("A".gen).baseType("A".gen)

  val Maybe = adt("Maybe").generic("A".gen)
    .constructors(
      cons("Nothing").generic("A".gen),
      cons("Just").generic("A".gen).field("get", "A".gen)
    )

  val Record = record("Record")
    .generic("A".gen)
    .field("id", "A".gen)
    .field("name", "Option".spec("string".spec))

  val Enum = enum("Enum").values(
    "A".en, "B".en, "C".en
  )

  val Either = adt("Either")
    .generic("E".gen, "A".gen)
    .constructors(
      cons("Left").generic("E".gen, "A".gen).field("value", "E".gen),
      cons("Right").generic("E".gen, "A".gen).field("value", "A".gen)
    )

  val Result = newtype("Result")
    .generic("A".gen)
    .baseType("Either".spec("string".spec, "A".gen))

  val packageAst = pkg("test.types")
    .types(
      MyInt,
      Id,
      Maybe,
      Record,
      Enum,
      Either,
      Result
    )
    .build

}
