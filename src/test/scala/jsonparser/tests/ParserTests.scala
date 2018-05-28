package jsonparser.tests
import org.scalatest.{FlatSpec, Matchers}
import jsonparser._

class ParserTests extends FlatSpec with Matchers {

  val parser = new JSONParser()

  "JSONParser" should "parse null" in {
    val res = parser.parseAll(parser.value, "null")
    res.successful should be (true)
    res.get shouldBe a [JNull]
  }

  it should "parse true" in {
    val res = parser.parseAll(parser.value, "true")
    res.successful should be (true)
    res.get should matchPattern { case b: JBool if b.value == true => }
  }

  it should "parse false" in {
    val res = parser.parseAll(parser.value, "false")
    res.successful should be (true)
    res.get should matchPattern { case b: JBool if b.value == false => }
  }

  it should "parse object" in {
    val res = parser.parseAll(parser.value, """{"hello": true, "world": false}""")
    val expected = JObject(Map("hello" -> JBool(true), "world" -> JBool(false)))
    res.successful should be (true)
    res.get shouldEqual expected
  }

  it should "parse nested object" in {
    val res = parser.parseAll(parser.value, """{"a": {"hello": true}, "world": false}""")
    val expected = JObject(Map("a" -> JObject(Map("hello" -> JBool(true))), "world" -> JBool(false)))
    res.successful should be(true)
    res.get shouldEqual expected
  }

  it should "parse array" in {
    val res = parser.parseAll(parser.array, """["hello", true, 5]""")
    val expected = JArray(List(JString("hello"), JBool(true), JNumber(5)))
    res.successful should be (true)
    res.get shouldEqual expected
  }

  it should "parse nested array" in {
    val res = parser.parseAll(parser.array, """[["hello", true, 5]]""")
    val expected = JArray(List(JArray(List(JString("hello"), JBool(true), JNumber(5)))))
    res.successful should be(true)
    res.get shouldEqual expected
  }

  it should "parse array of objects" in {
    val res = parser.parseAll(parser.array, """[[{"hello": true, "world": 5}]]""")
    val expected = JArray(List(JArray(List(JObject(Map( "hello" -> JBool(true), "world" -> JNumber(5)))))))
    res.successful should be (true)
    res.get shouldEqual expected
  }

  it should "parse empty array" in {
    val res = parser.parseAll(parser.array, """[]""")
    val expected = JArray(List())
    res.successful should be (true)
    res.get shouldEqual expected
  }

  it should "parse complex object" in {
    val json = """
  {
    "address book": {
        "name": "John Smith",
      "address": {
        "street": "10 Market Street",
        "city" : "San Francisco, CA",
        "zip" : 94111,
        "number" : 1.2345,
        "bool" : false,
        "null" : null
      },
      "phone numbers": [
        "408 338-4238",
        "408 111-6892"
      ]
    }
  }""";
    val res = parser.parseAll(parser.obj, json)
    val expected = JObject(Map("address book" -> JObject(Map("name" -> JString("John Smith"), "address" -> JObject(Map("number" -> JNumber(1.2345), "city" -> JString("San Francisco, CA"), "zip" -> JNumber(94111.0), "null" -> JNull(), "bool" -> JBool(false), "street" -> JString("10 Market Street"))), "phone numbers" -> JArray(List(JString("408 338-4238"), JString("408 111-6892")))))))
    res.successful should be (true)
    res.get shouldEqual expected
  }


}