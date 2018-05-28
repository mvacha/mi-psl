package jsonparser

abstract sealed class JValue

case class JObject(values : Map[String, JValue]) extends JValue
{
  override def equals(other: Any) = other match {
    case otherJObj: JObject =>
      values.keySet == otherJObj.values.keySet &&
        values.forall(item => otherJObj.values(item._1) == item._2)
    case _ => false
  }
}
case class JArray(items : List[JValue]) extends JValue
case class JString (value : String) extends JValue
case class JNumber(value : Double) extends JValue
case class JBool (value: Boolean) extends JValue
case class JNull () extends JValue
