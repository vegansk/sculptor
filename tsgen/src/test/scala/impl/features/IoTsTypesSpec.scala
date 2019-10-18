package sculptor.tsgen
package impl
package features

import org.specs2._
import cats.implicits._

object IoTsTypesSpec
    extends mutable.Specification
    with ScalaCheck
    with testing.Helpers {

  import sculptor.ast._
  import dsl._

  val feature = Feature.IoTsTypes(iotsNs = "t")
  val cfg = Config(
    features = List(feature),
    optionalEncoding = OptionalEncoding("Option"),
    generateAdtNs = false
  )

  "IoTsTypes" should {

    "handle simple aliases" >> {
      val a = alias("MyInt")
        .baseType("number".spec)
        .build

      runFeature(IoTsTypes(feature).handleAlias(a), cfg) must beEqvTo(
        """|export const MyIntType: t.Type<MyInt> = <any>t.number""".stripMargin.asRight
      )
    }

    "handle generic aliases" >> {
      val a = alias("Result")
        .generic("A".gen)
        .baseType("Either".spec("string".spec, "A".gen))
        .build

      runFeature(IoTsTypes(feature).handleAlias(a), cfg) must beEqvTo(
        """|export const ResultType: <A>(AType: t.Type<A>) => t.Type<Result<A>> = <A>(AType: t.Type<A>) => <any>EitherType(t.string, AType)""".stripMargin.asRight
      )
    }

    "handle simple newtypes" >> {
      val n = newtype("MyInt")
        .baseType("number".spec)
        .build

      runFeature(IoTsTypes(feature).handleNewtype(n), cfg) must beEqvTo(
        """|export const MyIntType: t.Type<MyInt> = <any>t.number""".stripMargin.asRight
      )
    }

    "handle generic newtypes" >> {
      val n = newtype("Result")
        .generic("A".gen)
        .baseType("Either".spec("string".spec, "A".gen))
        .build

      runFeature(IoTsTypes(feature).handleNewtype(n), cfg) must beEqvTo(
        """|export const ResultType: <A>(AType: t.Type<A>) => t.Type<Result<A>> = <A>(AType: t.Type<A>) => <any>EitherType(t.string, AType)""".stripMargin.asRight
      )
    }

    "handle packages" >> {
      val p = pkg("test")
        .types(
          record("Record")
            .field("id", "number".spec),
          enum("Enum")
            .values("A".en)
        )
        .build

      runFeature(IoTsTypes(feature).handlePackage(p), cfg) must beEqvTo(
        """|const typeImpl = <R extends t.Props, O extends t.Props>(r: R, o: O, name: string) =>
           |  t.intersection([t.interface(r), t.partial(o)], name)
           |
           |const getStringEnumValues = (o: object): string[] =>
           |  Object.keys(o).map(_ => (o as { [n: string]: any })[_]).filter(v => typeof v === "string")
           |
           |const stringEnumImpl = <E>(e: object, name: string): t.Type<E> => {
           |  const values = getStringEnumValues(e)
           |  return new t.Type<E>(
           |    name,
           |    (v): v is E => values.indexOf(v as string) >= 0,
           |    (v, c) => values.indexOf(v as string) >= 0 ? t.success<E>(v as E) : t.failure<E>(v, c),
           |    t.identity
           |  )
           |}""".stripMargin.asRight
      )
    }

    "handle simple records" >> {
      val r = record("Record")
        .field("id", "number".spec)
        .field("name", "Option".spec("string".spec))
        .build

      runFeature(IoTsTypes(feature).handleRecord(r), cfg) must beEqvTo(
        """export const RecordType: t.Type<Record> = typeImpl({id: t.number}, {name: t.string}, "Record")""".asRight
      )
    }

    "handle generic records" >> {
      val r = record("Record")
        .generic("A".gen)
        .field("id", "A".gen)
        .field("name", "Option".spec("string".spec))
        .build

      runFeature(IoTsTypes(feature).handleRecord(r), cfg) must beEqvTo(
        """|export const RecordType: <A>(AType: t.Type<A>) => t.Type<Record<A>> = <A>(AType: t.Type<A>) => typeImpl(
           |  {id: AType}, {name: t.string}, "Record"
           |)""".stripMargin.asRight
      )
    }

    "handle ADTs" >> {
      val a = adt("Maybe")
        .generic("A".gen)
        .constructors(
          cons("Empty").generic("A".gen),
          cons("Just")
            .generic("A".gen)
            .field("value", "A".gen)
        )
        .build

      "without namespaces" >> {
        runFeature(IoTsTypes(feature).handleADT(a), cfg) must beEqvTo(
          """|export const EmptyType: <A>(AType: t.Type<A>) => t.Tagged<"__tag", Empty<A>> = <A>(AType: t.Type<A>) => typeImpl(
             |  {__tag: t.literal("Empty")}, {}, "Empty"
             |)
             |
             |export const JustType: <A>(AType: t.Type<A>) => t.Tagged<"__tag", Just<A>> = <A>(AType: t.Type<A>) => typeImpl(
             |  {__tag: t.literal("Just"), value: AType}, {}, "Just"
             |)
             |
             |export const MaybeType: <A>(AType: t.Type<A>) => t.Type<Maybe<A>> = <A>(AType: t.Type<A>) => t.taggedUnion("__tag", [
             |  EmptyType(AType), JustType(AType)
             |], "Maybe")""".stripMargin.asRight
        )
      }

      "with namespaces" >> {
        runFeature(
          IoTsTypes(feature).handleADT(a),
          cfg.copy(generateAdtNs = true)
        ) must beEqvTo(
          """|export namespace Maybe {
             |  export const EmptyType: <A>(AType: t.Type<A>) => t.Tagged<"__tag", Empty<A>> = <A>(AType: t.Type<A>) => typeImpl(
             |    {__tag: t.literal("Empty")}, {}, "Empty"
             |  )
             |
             |  export const JustType: <A>(AType: t.Type<A>) => t.Tagged<"__tag", Just<A>> = <A>(AType: t.Type<A>) => typeImpl(
             |    {__tag: t.literal("Just"), value: AType}, {}, "Just"
             |  )
             |}
             |
             |export const MaybeType: <A>(AType: t.Type<A>) => t.Type<Maybe<A>> = <A>(AType: t.Type<A>) => t.taggedUnion("__tag", [
             |  Maybe.EmptyType(AType), Maybe.JustType(AType)
             |], "Maybe")""".stripMargin.asRight
        )
      }
    }

    "handle enums" >> {
      val e = enum("Enum")
        .values("A".en, "B".en)
        .build

      runFeature(IoTsTypes(feature).handleEnum(e), cfg) must beEqvTo(
        """export const EnumType: t.Type<Enum> = stringEnumImpl<Enum>(Enum, "Enum")""".asRight
      )
    }

    "allow to use custom iots types" >> {

      val a = adt("Test")
        .constructors(cons("A"), cons("B"))
        .build

      val feature0 = feature.copy(
        customIotsType = "MyType",
        customIotsTaggedType = "MyTaggedType"
      )

      runFeature(
        IoTsTypes(feature0).handleADT(a),
        cfg.copy(features = List(feature0))
      ) must beEqvTo(
        """|export const AType: MyTaggedType<"__tag", A> = typeImpl({__tag: t.literal("A")}, {}, "A")
           |
           |export const BType: MyTaggedType<"__tag", B> = typeImpl({__tag: t.literal("B")}, {}, "B")
           |
           |export const TestType: MyType<Test> = t.taggedUnion("__tag", [AType, BType], "Test")""".stripMargin.asRight
      )
    }

    "allow to compose types mappings" >> {

      val a = alias("Test").baseType("Map".spec("A".gen, "B".gen)).build

      val mapping: IotsMapping = {
        case ("Map", ns) => s"${ns}dictionary"
      }

      val feature0 = feature.copy(typeMapping = mapping)

      runFeature(
        IoTsTypes(feature0).handleAlias(a),
        cfg.copy(features = List(feature0))
      ) must beEqvTo(
        """export const TestType: t.Type<Test> = <any>t.dictionary(AType, BType)""".asRight
      )

    }
  }
}
