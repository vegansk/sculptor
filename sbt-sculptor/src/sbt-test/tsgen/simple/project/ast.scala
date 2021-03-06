import sculptor.ast.dsl._

object Simple {

  val MyInt = newtype("MyInt").baseType("number".spec)

  val Id = alias("Id").generic("A".gen).baseType("A".gen)

  val Maybe = adt("Maybe").generic("A".gen)
    .constructors(
      cons("Nothing").generic("A".gen),
      cons("Just").generic("A".gen).field("get", "A".gen, "The value")
    )

  val Record = record("Record")
    .generic("A".gen)
    .field("id", Id.build.ref("A".gen), "The id")
    .field("name", "Option".spec("string".spec), "The name")
    .field("age", "number".spec, "The age")

  val RecordWithSingleField = record("RecordWithSingleField")
    .field("id", "number".spec, "Single field record")

  val Enum = enum("Enum").values(
    "A".en, "B".en, "C".en
  )

  val Either = adt("Either")
    .generic("E".gen, "A".gen)
    .constructors(
      cons("Left").generic("E".gen, "A".gen).field("value", "E".gen, "Left value"),
      cons("Right").generic("E".gen, "A".gen).field("value", "A".gen, "Right value")
    )

  val Result = newtype("Result")
    .generic("A".gen)
    .baseType("Either".spec("string".spec, "A".gen))

  val AdtWithOneElement = adt("AdtWithOneElement")
    .constructors(
      cons("Element")
    )

  val packageAst = pkg("test.types")
    .types(
      MyInt,
      Record,
      Id,
      Maybe,
      RecordWithSingleField,
      Enum,
      Either,
      Result,
      AdtWithOneElement
    )
    .build

}
