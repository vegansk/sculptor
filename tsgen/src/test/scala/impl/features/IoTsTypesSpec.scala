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
        """export const MyIntType: t.Type<MyInt> = <any>t.number""".fix.asRight
      )
    }

    "handle generic aliases" >> {
      val a = alias("Result")
        .generic("A".gen)
        .baseType("Either".spec("string".spec, "A".gen))
        .build

      runFeature(IoTsTypes(feature).handleAlias(a), cfg) must beEqvTo(
        """export const ResultType: <A>(AType: t.Type<A>) => t.Type<Result<A>> = <A>(AType: t.Type<A>) => <any>EitherType(t.string, AType)""".fix.asRight
      )
    }

    "handle simple newtypes" >> {
      val n = newtype("MyInt")
        .baseType("number".spec)
        .build

      runFeature(IoTsTypes(feature).handleNewtype(n), cfg) must beEqvTo(
        """export const MyIntType: t.Type<MyInt> = <any>t.number""".fix.asRight
      )
    }

    "handle generic newtypes" >> {
      val n = newtype("Result")
        .generic("A".gen)
        .baseType("Either".spec("string".spec, "A".gen))
        .build

      runFeature(IoTsTypes(feature).handleNewtype(n), cfg) must beEqvTo(
        """export const ResultType: <A>(AType: t.Type<A>) => t.Type<Result<A>> = <A>(AType: t.Type<A>) => <any>EitherType(t.string, AType)""".fix.asRight
      )
    }

    "handle packages" >> {
      val enum0 = enum("Enum")
        .values("A".en)
        .build
      val p = pkg("test")
        .types(
          record("Record")
            .field("v", enum0.ref)
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
           |}""".fix.asRight
      )
    }

    "handle simple records" >> {
      val r = record("Record")
        .field("id", "number".spec)
        .field("name", "Option".spec("string".spec))
        .build

      runFeature(IoTsTypes(feature).handleRecord(r), cfg) must beEqvTo(
        """export const RecordType: t.Type<Record> = typeImpl({id: t.number}, {name: t.string}, "Record")""".fix.asRight
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
           |)""".fix.asRight
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

        "without export" >> {
          runFeature(IoTsTypes(feature).handleADT(a), cfg) must beEqvTo(
            """|const EmptyType: <A>(AType: t.Type<A>) => t.Tagged<"__tag", Empty<A>> = <A>(AType: t.Type<A>) => typeImpl(
              |  {__tag: t.literal("Empty")}, {}, "Empty"
              |)
              |
              |const JustType: <A>(AType: t.Type<A>) => t.Tagged<"__tag", Just<A>> = <A>(AType: t.Type<A>) => typeImpl(
              |  {__tag: t.literal("Just"), value: AType}, {}, "Just"
              |)
              |
              |export const MaybeType: <A>(AType: t.Type<A>) => t.Type<Maybe<A>> = <A>(AType: t.Type<A>) => t.union([
              |  EmptyType(AType), JustType(AType)
              |], "Maybe")""".fix.asRight
          )
        }

        "with export" >> {
          runFeature(
            IoTsTypes(feature.copy(exportADTConstructorsCodecs = true))
              .handleADT(a),
            cfg
          ) must beEqvTo(
            """|export const EmptyType: <A>(AType: t.Type<A>) => t.Tagged<"__tag", Empty<A>> = <A>(AType: t.Type<A>) => typeImpl(
              |  {__tag: t.literal("Empty")}, {}, "Empty"
              |)
              |
              |export const JustType: <A>(AType: t.Type<A>) => t.Tagged<"__tag", Just<A>> = <A>(AType: t.Type<A>) => typeImpl(
              |  {__tag: t.literal("Just"), value: AType}, {}, "Just"
              |)
              |
              |export const MaybeType: <A>(AType: t.Type<A>) => t.Type<Maybe<A>> = <A>(AType: t.Type<A>) => t.union([
              |  EmptyType(AType), JustType(AType)
              |], "Maybe")""".fix.asRight
          )
        }

      }

      "with namespaces" >> {

        "without export" >> {
          runFeature(
            IoTsTypes(feature).handleADT(a),
            cfg.copy(generateAdtNs = true)
          ) must beEqvTo(
            """|const MaybeCodecs = {
              |  EmptyType: <A>(AType: t.Type<A>): t.Tagged<"__tag", Maybe.Empty<A>> => typeImpl(
              |    {__tag: t.literal("Empty")}, {}, "Empty"
              |  ),
              |  
              |  JustType: <A>(AType: t.Type<A>): t.Tagged<"__tag", Maybe.Just<A>> => typeImpl(
              |    {__tag: t.literal("Just"), value: AType}, {}, "Just"
              |  )
              |}
              |
              |export const MaybeType: <A>(AType: t.Type<A>) => t.Type<Maybe<A>> = <A>(AType: t.Type<A>) => t.union([
              |  MaybeCodecs.EmptyType(AType), MaybeCodecs.JustType(AType)
              |], "Maybe")""".fix.asRight
          )
        }

        "with export" >> {
          runFeature(
            IoTsTypes(feature.copy(exportADTConstructorsCodecs = true))
              .handleADT(a),
            cfg.copy(generateAdtNs = true)
          ) must beEqvTo(
            """|export const MaybeCodecs = {
              |  EmptyType: <A>(AType: t.Type<A>): t.Tagged<"__tag", Maybe.Empty<A>> => typeImpl(
              |    {__tag: t.literal("Empty")}, {}, "Empty"
              |  ),
              |  
              |  JustType: <A>(AType: t.Type<A>): t.Tagged<"__tag", Maybe.Just<A>> => typeImpl(
              |    {__tag: t.literal("Just"), value: AType}, {}, "Just"
              |  )
              |}
              |
              |export const MaybeType: <A>(AType: t.Type<A>) => t.Type<Maybe<A>> = <A>(AType: t.Type<A>) => t.union([
              |  MaybeCodecs.EmptyType(AType), MaybeCodecs.JustType(AType)
              |], "Maybe")""".fix.asRight
          )
        }
      }

      "with one element" >> {
        runFeature(
          IoTsTypes(feature)
            .handleADT(adt("Test").constructors(cons("Cons")).build),
          cfg
        ) must beEqvTo(
          """|const ConsType: t.Tagged<"__tag", Cons> = typeImpl({__tag: t.literal("Cons")}, {}, "Cons")
             |
             |export const TestType: t.Type<Test> = ConsType""".fix.asRight
        )
      }

      "with namespaces, without generics" >> {
        runFeature(
          IoTsTypes(feature)
            .handleADT(adt("Test").constructors(cons("Cons")).build),
          cfg.copy(generateAdtNs = true)
        ) must beEqvTo(
          """|const TestCodecs = {ConsType: typeImpl({__tag: t.literal("Cons")}, {}, "Cons") as t.Tagged<"__tag", Test.Cons>}
             |
             |export const TestType: t.Type<Test> = TestCodecs.ConsType""".fix.asRight
        )
      }
    }

    "handle enums" >> {
      val e = enum("Enum")
        .values("A".en, "B".en)
        .build

      runFeature(IoTsTypes(feature).handleEnum(e), cfg) must beEqvTo(
        """export const EnumType: t.Type<Enum> = stringEnumImpl<Enum>(Enum, "Enum")""".fix.asRight
      )
    }

    "allow to use custom iots types" >> {

      val a = adt("Test")
        .generic("X".gen)
        .constructors(cons("A").generic("X".gen), cons("B").generic("X".gen))
        .build

      val feature0 = feature.copy(
        customIotsType = "MyType",
        customIotsTaggedType = "MyTaggedType"
      )

      runFeature(
        IoTsTypes(feature0).handleADT(a),
        cfg.copy(features = List(feature0))
      ) must beEqvTo(
        """|const AType: <X>(XType: MyType<X>) => MyTaggedType<"__tag", A<X>> = <X>(XType: MyType<X>) => typeImpl(
           |  {__tag: t.literal("A")}, {}, "A"
           |)
           |
           |const BType: <X>(XType: MyType<X>) => MyTaggedType<"__tag", B<X>> = <X>(XType: MyType<X>) => typeImpl(
           |  {__tag: t.literal("B")}, {}, "B"
           |)
           |
           |export const TestType: <X>(XType: MyType<X>) => MyType<Test<X>> = <X>(XType: MyType<X>) => t.union([
           |  AType(XType), BType(XType)
           |], "Test")""".fix.asRight
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
        """export const TestType: t.Type<Test> = <any>t.dictionary(AType, BType)""".fix.asRight
      )

    }

    "produce correct type mappings (strange issue after merge with master)" >> {
      val r = record("RecordWithSingleField")
        .field("id", "number".spec, "Single field record")
        .build

      runFeature(IoTsTypes(feature).handleRecord(r), cfg) must beEqvTo(
        """export const RecordWithSingleFieldType: t.Type<RecordWithSingleField> = typeImpl(
          |  {id: t.number}, {}, "RecordWithSingleField"
          |)""".fix.asRight
      )
    }

    "produce correct records with only optional fields" >> {
      val r = record("Test")
        .field("id", "Option".spec("number".spec))
        .build

      runFeature(IoTsTypes(feature).handleRecord(r), cfg) must beEqvTo(
        """export const TestType: t.Type<Test> = typeImpl({}, {id: t.number}, "Test")""".fix.asRight
      )
    }
  }
}
