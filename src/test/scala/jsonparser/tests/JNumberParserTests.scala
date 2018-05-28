package jsonparser.tests
import org.scalatest.{FlatSpec, Matchers}
import jsonparser._

class JNumberParserTests extends FlatSpec with Matchers {
  val parser = new JSONParser()

  "JNumber parser" should "parse 0" in {
    val res = parser.parseAll(parser.number, "0")
    res.successful should be (true)
    res.get should matchPattern { case b: JNumber if b.value == 0 => }
  }

  it should "parse integer" in {
    val res = parser.parseAll(parser.number, "123456789")
    res.successful should be (true)
    res.get should matchPattern { case b: JNumber if b.value == 123456789 => }
  }

  it should "parse negative integer" in {
    val res = parser.parseAll(parser.number, "-123456789")
    res.successful should be (true)
    res.get should matchPattern { case b: JNumber if b.value == -123456789 => }
  }

  it should "parse decimal" in {
    val res = parser.parseAll(parser.number, "0.2")
    res.successful should be (true)
    res.get should matchPattern { case b: JNumber if b.value == 0.2 => }
  }

  it should "parse negative decimal" in {
    val res = parser.parseAll(parser.number, "-0.2")
    res.successful should be (true)
    res.get should matchPattern { case b: JNumber if b.value == -0.2 => }
  }

  it should "parse number with exponent" in {
    val res = parser.parseAll(parser.number, "2E+5")
    res.successful should be (true)
    res.get should matchPattern { case b: JNumber if b.value == 2e5 => }
  }

  it should " not parse number with decimal exponent" in {
    val res = parser.parseAll(parser.number, "2E5.124")
    res.successful should be (false)
  }

}
