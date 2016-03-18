package sangria.marshalling

import org.msgpack.value.ValueFactory
import org.scalatest.{Matchers, WordSpec}

import sangria.marshalling.testkit._

class MsgpackSupportSpec extends WordSpec with Matchers with MarshallingBehaviour with InputHandlingBehaviour with ParsingBehaviour {
  import sangria.marshalling.msgpack._

  "Msgpack integration" should {
    behave like `value (un)marshaller` (msgpackResultMarshaller)

    behave like `AST-based input unmarshaller` (msgpackFromInput)
    behave like `AST-based input marshaller` (msgpackResultMarshaller)

    behave like `input parser` (ParseTestSubjects(
      complex = "gqFhk8B7kYGjZm9vo2JhcqFigqFjw6FkwA==",
      simpleString = "o2Jhcg==",
      simpleInt = "zTA5",
      simpleNull = "wA==",
      list = "laNiYXIBwMOTAQID",
      syntaxError = List("jhdskjfdskjfkhdsg")
    ))
  }

  val toRender = ValueFactory.newMap(
    ValueFactory.newString("a"), ValueFactory.newArray(
      ValueFactory.newNil(), ValueFactory.newInteger(123), ValueFactory.newArray(
        ValueFactory.newMap(ValueFactory.newString("foo"), ValueFactory.newString("bar")))),
    ValueFactory.newString("b"), ValueFactory.newMap(
      ValueFactory.newString("c"), ValueFactory.newBoolean(true),
      ValueFactory.newString("d"), ValueFactory.newNil()))

  "InputUnmarshaller" should {
    "throw an exception on invalid scalar values" in {
      an [IllegalStateException] should be thrownBy
          msgpackInputUnmarshaller.getScalarValue(ValueFactory.emptyMap())
    }

    "throw an exception on variable names" in {
      an [IllegalArgumentException] should be thrownBy
          msgpackInputUnmarshaller.getVariableName(ValueFactory.newString("$foo"))
    }

    "render JSON values" in {
      val rendered = msgpackInputUnmarshaller.render(toRender)

      toRender.toJson should be("""{"a":[null,123,[{"foo":"bar"}]],"b":{"c":true,"d":null}}""")
      rendered should be ("gqFhk8B7kYGjZm9vo2JhcqFigqFjw6FkwA==")
    }
  }

  "ResultMarshaller" should {
    "render JSON values" in {
      val renderedCompact = msgpackResultMarshaller.renderCompact(toRender)
      val renderedPretty = msgpackResultMarshaller.renderCompact(toRender)

      renderedCompact should be (renderedPretty)

      renderedCompact should be ("gqFhk8B7kYGjZm9vo2JhcqFigqFjw6FkwA==")
    }
  }
}
