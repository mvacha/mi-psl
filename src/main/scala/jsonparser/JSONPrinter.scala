package jsonparser

object JSONPrinter {
  private def format(v: JValue, sb: StringBuilder): Unit = v match {
      case JNull() => sb.append("null")
      case JString(str) => sb.append("\"" + str + "\"")
      case JBool(value) => sb.append(if (value) "true" else "false")
      case JNumber(value) => sb.append(value.toString)
      case JArray(list) =>
        sb.append("[")
        list.foreach(l =>  {
          if (l != list.head)
            sb.append(", ")
          format(l, sb)
        })
        sb.append("]")

      case JObject(map) =>
        sb.append("{")
        map.foreach(item => {
          if (item != map.head)
            sb.append(", ")
          sb.append("\"" + item._1 + "\":")
          format(item._2, sb)
        })
        sb.append("}")
    }
  }

  def print(v: JValue) = {
    val sb = new StringBuilder()
    format(v, sb)
    sb.toString
  }
}
