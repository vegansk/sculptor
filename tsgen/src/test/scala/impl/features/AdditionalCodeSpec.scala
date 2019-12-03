package sculptor.tsgen
package impl
package features

import org.specs2._
import cats.implicits._

object AdditionalCodeSpec
    extends mutable.Specification
    with ScalaCheck
    with testing.Helpers {

  import sculptor.ast._
  import dsl._

  val cfg = Config(
    features = List(Feature.AdditionalCode),
    optionalEncoding = OptionalEncoding("Option"),
    generateAdtNs = false
  )

  "AdditionalCode" should {

    "handle aliases" >> {
      val a = alias("MyInt")
        .baseType("number".spec)
        .additionalCodeS("// Additional comment")
        .build

      runFeature(AdditionalCode.handleAlias(a), cfg) must beEqvTo(
        """// Additional comment""".fix.asRight
      )
    }

    "handle simple newtypes" >> {
      val n = newtype("MyInt")
        .baseType("number".spec)
        .additionalCodeS("// Additional comment")
        .build

      runFeature(AdditionalCode.handleNewtype(n), cfg) must beEqvTo(
        """// Additional comment""".fix.asRight
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
        .additionalCodeS("// Additional comment")
        .build

      runFeature(AdditionalCode.handlePackage(p), cfg) must beEqvTo(
        """// Additional comment""".fix.asRight
      )
    }

    "handle records" >> {
      val r = record("Record")
        .field("id", "number".spec)
        .field("name", "Option".spec("string".spec))
        .additionalCodeS("// Additional comment")
        .build

      runFeature(AdditionalCode.handleRecord(r), cfg) must beEqvTo(
        """// Additional comment""".fix.asRight
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
        .additionalCodeS("// Additional comment")
        .build

      runFeature(AdditionalCode.handleADT(a), cfg) must beEqvTo(
        """// Additional comment""".fix.asRight
      )
    }

    "handle enums" >> {
      val e = enum("Enum")
        .values("A".en, "B".en)
        .additionalCodeS("// Additional comment")
        .build

      runFeature(AdditionalCode.handleEnum(e), cfg) must beEqvTo(
        """// Additional comment""".fix.asRight
      )
    }
  }
}
