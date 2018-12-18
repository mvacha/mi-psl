import jsonparser._

object ParseExpr extends Arith {
  def main(args: Array[String]) = {
    val str = "0 + 9 * 1"
    println("input : " + str)
    println(parseAll(expr, str))
  }
}

object IdentExpr extends MyParsers {
  def main(args: Array[String]) = {
    val str = "MyParsers"
    println(parseAll(ident, str))
    println("""[a-z]+""".r.findFirstMatchIn("aaa").get);
  }
}

object TestSimpleParser extends SimpleParser {
  def main(args: Array[String]) = {
    parse(freq, "johnny124") match {
      case Success(matched, _) => println(matched)
      case Failure(error, _) => print("FAILURE: " + error)
      case Error(msg, _) => print("ERROR: " + msg)
    }
  }
}

object ParseJSON extends JSONParser {
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
  }"""
  def main(args: Array[String]) = {
    val str = JSONPrinter.print(parseAll(value, json).get)
    println(parseAll(value, str))
  }
}