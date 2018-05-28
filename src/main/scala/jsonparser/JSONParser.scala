package jsonparser

import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

class JSONParser extends JavaTokenParsers {
  def value : Parser[JValue] = (
      obj
    | array
    | string
    | number
    | "null" ^^ (_ => JNull())
    | "true" ^^ (_ => JBool(true))
    | "false" ^^ (_ => JBool(false))
    )

  def obj : Parser[JObject] = "{" ~> repsep(member, ",") <~ "}" ^^ (x => JObject(x.toMap))

  def array : Parser[JArray] = "[" ~> repsep(value, ",") <~ "]" ^^ (x=> JArray(x))

  def member : Parser[(String, JValue)] = string~":"~value ^^ { case name~":"~value => (name.value, value)}

  //TODO: broken - does not match whitespace

  //All unicode characters except '\' and '"', square brackets cannot be represented by their unicode code as they break the regex
//  def unescaped : Parser[String] = """[\u0000-\u0021\u0023-\[\]-\uffff]""".r ^^ (_.toString)
//
//  def escape : Parser[String] = "\\"
//
//  def char : Parser[String] = unescaped | (escape ~ """[\u0022\u005c\u002f\u0062\u0066\u006e\u0072\u0074\u0075]""".r) ^^ (_.toString)
//
//  def string : Parser[JString] =  "\"" ~> rep(char) <~ '"' ^^ (x => JString(x.toString))

  //https://stackoverflow.com/questions/32155133/regex-to-match-a-json-string
  def string : Parser[JString] =  """("(((?=\\)\\(["\\\/bfnrt]|u[0-9a-fA-F]{4}))|[^"\\\x00-\x1F\x7F]+)*")""".r ^^ (x => JString(StringContext treatEscapes x.toString.stripPrefix("\"").stripSuffix("\"")))

  def number : Parser[JNumber] = optToStr("-") ~ int ~ optToStr(frac) ~ optToStr(exp) ^^ {case x1~x2~x3~x4 => JNumber(x1 + x2 + x3 + x4 toDouble)}

  def digits : Parser[String] = rep1("""[1-9]""".r) ^^ (_ mkString "")

  def e : Parser[String] = "e" | "E"

  def exp : Parser[String] = e ~ optToStr("-" | "+") ~ digits ^^ {case e1~e2~e3 => e1 + e2 + e3}

  def frac : Parser[String] = "." ~ digits ^^ {case _ ~ n => "." + n}

  def int : Parser[String] = "0" | digits

  def optToStr(parser: Parser[String]) : Parser[String] = opt(parser) ^^ {case Some(s) => s.toString case _ => ""}

}