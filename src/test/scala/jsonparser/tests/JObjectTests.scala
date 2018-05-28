package jsonparser.tests
import org.scalatest.{FlatSpec, Matchers}
import jsonparser._

class JObjectTests  extends FlatSpec with Matchers {
  "JObjects wih same members" should "be equal" in {
    val jObj1 = JObject(Map("1" -> JNumber(1), "2" -> JNumber(2)))
    val jObj2 = JObject(Map("1" -> JNumber(1), "2" -> JNumber(2)))
    jObj1 shouldEqual jObj2
    jObj2 shouldEqual jObj1
  }
  "JObjects wih same members in different order" should "be equal" in {
    val jObj1 = JObject(Map("1" -> JNumber(1), "2" -> JNumber(2)))
    val jObj2 = JObject(Map("2" -> JNumber(2), "1" -> JNumber(1)))
    jObj1 shouldEqual jObj2
    jObj2 shouldEqual jObj1
  }
  "JObjects wih different member value" should "not be equal" in {
    val jObj1 = JObject(Map("1" -> JNumber(1), "2" -> JNumber(2)))
    val jObj2 = JObject(Map("1" -> JNumber(2), "2" -> JNumber(1)))
    jObj1 shouldNot equal (jObj2)
    jObj2 shouldNot equal (jObj1)
  }
}
