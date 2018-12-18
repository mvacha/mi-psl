package jsonparser.tests
import org.scalatest.{FlatSpec, Matchers}
import jsonparser._

trait Matchers2 {

}

class PrinterTests extends FlatSpec with Matchers2 {

  val parser = new JSONParser()

  "JSON Printer" should "parse print type which is than parsable as the same object" in {
    val origJson = """
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
  }"""
    val res = parser.parseAll(parser.obj, origJson)
    val expected = JObject(Map("address book" -> JObject(Map("name" -> JString("John Smith"), "address" -> JObject(Map("number" -> JNumber(1.2345), "city" -> JString("San Francisco, CA"), "zip" -> JNumber(94111.0), "null" -> JNull(), "bool" -> JBool(false), "street" -> JString("10 Market Street"))), "phone numbers" -> JArray(List(JString("408 338-4238"), JString("408 111-6892")))))))
    val newJson = JSONPrinter.print(res.get)
    val newRes = parser.parseAll(parser.obj, newJson)
    res.successful should be (true)
    newRes.get shouldEqual expected
  }

}