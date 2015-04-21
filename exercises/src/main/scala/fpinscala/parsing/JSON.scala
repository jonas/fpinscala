package fpinscala.parsing

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def parser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P.{string => _, _}
    implicit val token = P.token _

    val string: Parser[String] = {
      val quote   = char('"')
      val hex     = regex("[0-9a-fA-F]".r)
      val escChar = char('"') | char('\\') | char('/') | char('b') |
                    char('t') | char('n') | char('r') | char('t') |
	            listOfN(4, hex)
      val charEsc = char('\\') ~ escChar
      val charLit = regex("[^\"\\\\]".r)
      val letters  = (charLit | charEsc).many.slice

      (quote *> letters <* quote)
    }

    val jnull:   Parser[JSON]    = "null".as(JNull)

    val jnumber: Parser[JNumber] = {
      val sign    = char('-').?
      val zero    = char('0')
      val digit1  = regex("[1-9]".r)
      val digit   = regex("[0-9]".r)

      val integer = sign ~ (zero | (digit1 ~ digit.*))
      val decimal = char('.') ~ digit.+
      val exp     = (char('e') | char('E')) ~ (char('+') | char('-')).? ~ digit.+

      val number  = (integer ~ decimal.? ~ exp.?).slice map (_.toDouble)

      number map (JNumber(_))
    }

    val jstring: Parser[JString] = string map JString.apply

    val jbool:   Parser[JBool]   = "true".as(JBool(true)) | "false".as(JBool(false))

    def value :  Parser[JSON]    = (jnull | jnumber | jstring | jbool | jarray | jobject) <* whitespace

    def jobject: Parser[JObject] = {
      val prop: Parser[(String, JSON)] = (string <* whitespace <* ":") ~ value

      surround("{", "}")(prop sep ",") map (props => JObject(props.toMap))
    }

    def jarray:  Parser[JArray]  =
      surround("[", "]")(value sep ",") map (list => JArray(list.toIndexedSeq))

    whitespace *> (jobject | jarray)
  }
}
