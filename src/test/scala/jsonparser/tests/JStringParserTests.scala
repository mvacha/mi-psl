package jsonparser.tests
import org.scalatest.{FlatSpec, Matchers}
import jsonparser._

class JStringParserTests extends FlatSpec with Matchers {
  val parser = new JSONParser()

  "JString" can "contain emoji" in {
    val res = parser.parseAll(parser.string, "\"\uD83D\uDE0B\"") //😋
    res.successful should be (true)
    res.get should matchPattern { case b: JString if b.value == "\uD83D\uDE0B" => }
  }

  it can "be just space" in {
    val res = parser.parseAll(parser.string, "\" \"")
    res.successful should be (true)
    res.get should matchPattern { case b: JString if b.value == " " => }
  }

  "JString parser" should "handle escaped characters" in {
    val res = parser.parseAll(parser.string, "\"\\\"\"") // \" -> "
    res.successful should be (true)
    res.get should matchPattern { case b: JString if b.value == "\"" => }
  }

  it should "handle spaces, escaped characters and emojis" in {
    val res = parser.parseAll(parser.string, "\" \uD83D\uDE0B \\\"\"")
    res.successful should be (true)
    res.get should matchPattern { case b: JString if b.value == " \uD83D\uDE0B \"" => }
  }

  it should "not parse unknown escape sequences" in {
    val res = parser.parseAll(parser.string, "\" \\# \"")
    res.successful should be (false)
  }

}
