package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def map[A,B](pa: Parser[A])(f: A => B): Parser[B] =
    flatMap(pa)(a => unit(f(a)))

  def unit[A](a: A): Parser[A] =
    string("") map (_ => a)

  def map2[A,B,C](pa: Parser[A], pb: => Parser[B])(f: (A,B) => C): Parser[C] =
    flatMap(pa)(a => map(pb)(b => f(a, b)))

  def product[A,B](pa: Parser[A], pb: => Parser[B]): Parser[(A,B)] =
    flatMap(pa)(a => map(pb)((a, _)))

  def negate[A](p: Parser[A]): Parser[A]
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def regex(r: Regex): Parser[String]

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def optional[A](p: Parser[A]): Parser[Option[A]] =
    or(map(p)(a => Some(a)), unit(None))

  def many[A](p: Parser[A]): Parser[List[A]] =
    or(map2(p, many(p))(_ :: _), unit(Nil))

  def many1[A](p: Parser[A]) =
    map2(p, many(p))(_ :: _)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0)
      unit(Nil)
    else
      map2(p, listOfN(n - 1, p))(_ :: _)

  def slice[A](p: Parser[A]): Parser[String]

  def symbol[A](s: String, a: A): Parser[A] =
    string(s) map (_ => a)

  case class ParserOps[A](p: Parser[A]) {
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def ~[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p,p2)
    def **[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p,p2)
    def product[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p,p2)
    def unary_![B>:A](): Parser[B] = self.negate(p)
    def not[B>:A](): Parser[B] = self.negate(p)
    def ?[B>:A](): Parser[Option[B]] = self.optional(p)
    def *[B>:A](): Parser[List[B]] = self.many(p)
    def many[B>:A](): Parser[List[B]] = self.many(p)
    def +[B>:A](): Parser[List[B]] = self.many1(p)
    def slice(): Parser[String] = self.slice(p)
  }

  object Laws { self =>

    def equals[A](p1: Parser[A], p2: Parser[A])(input: Gen[String]): Prop =
      forAll(input)(input => run(p1)(input) == run(p2)(input))

    def means[A](p1: Parser[A], p2: Parser[A])(input: Gen[String]): Prop =
      forAll(input)(input => if (run(p1)(input).isRight) run(p2)(input).isRight else true)

    def equals[A](p: Parser[A], r: Either[ParseError,A])(input: Gen[String]): Prop =
      forAll(input)(input => run(p)(input) == r)

    case class EvalOps[A](p: Parser[A]) {
      def ===[B>:A](p2: => Parser[B]) = self.equals(p, p2) _
      def ===[B>:A](p2: Either[ParseError,B]) = self.equals(p, p2) _
      def ==>[B>:A](p2: => Parser[B]) = self.means(p, p2) _
    }
    implicit def evaluator[A](p: Parser[A]) = EvalOps[A](p)
    implicit def right[A](a: A) = Right(a)

    def mapIdentity[A](p: Parser[A]) =
      map(p)(a => a) === p

    def unitLaw[A](a: A) =
      unit(a) === a

    def orIsAssociative[A](a: Parser[A], b: Parser[A], c: Parser[A]) =
      ((a | b) | c) === (a | (b | c))

    def orIsCommutative[A](a: Parser[A], b: Parser[A]) =
      (a | b) === (b | a)

    // Exercise 9.2
    def productIsAssociative[A](a: Parser[A], b: Parser[A], c: Parser[A]) =
      ((a ~ b) ~ c) === (a ~ (b ~ c))

    def productDistributesOverOr[A](a: Parser[A], b: Parser[A], c: Parser[A]) =
      (a ~ (b | c)) === ((a ~ b) | (a ~ c))

    def notIsDistributive[A](a: Parser[A], b: Parser[A]) =
      (! (a | b)) === ((!b) | (!a))

    def many1MeansManyMatches[A](a: Parser[A]) =
      (a +) ==> (a *)

    def optionalMeansManyMatches[A](a: Parser[A]) =
      (a ?) ==> (a *)

    /** Spec **/
    def manyMapSize =
      run((char('a') *).map(_.size))("aaa") == Right(3)

    def manyMapSize2 =
      run((char('a') *).slice.map(_.size))("aaa") == Right(3)

    def sliceLaw =
      run(slice((char('a') | char('b')) *))("aaba") == Right("aaba")

    def lengthEncodedStringParser =
      "[0-9]".r flatMap (length => listOfN(length.toInt, "a"))

    def lengthEncodedStringParser(input: Gen[String]): Prop =
      forAll(input)(input => {
        val trimmed = input.substring(0, math.min(9, input.length))
        val encoded = s"${trimmed.length}${trimmed}"

        run(lengthEncodedStringParser)(encoded) == Right(encoded)
      })

  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def parser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    val spaces = char(' ').many.slice
    def token(c: Char) = (spaces ~ char(c) ~ spaces).slice

    val stringLit: Parser[String] = {
      val quote   = char('"')
      val hex     = regex("[0-9a-fA-F]".r)
      val escChar = char('"') | char('\\') | char('/') | char('b') |
                    char('t') | char('n') | char('r') | char('t') |
	            listOfN(4, hex)
      val charEsc = char('\\') ~ escChar
      val charLit = ! (char('"') | char('\\'))
      val letters  = (charLit | charEsc).many.slice

      (quote ~ letters ~ quote) map (_._1._2)
    }

    val jnull:   Parser[JSON]    = symbol("null", JNull)

    val jnumber: Parser[JNumber] = {
      val sign    = char('-').?
      val zero    = char('0')
      val digit1  = regex("[1-9]".r)
      val digit   = regex("[0-9]".r)

      val integer = sign ~ (zero | (digit1 ~ digit.*))
      val decimal = char('.') ~ digit.+
      val exp     = (char('e') | char('E')) ~ (char('+') | char('-')).? ~ digit.+

      val number  = integer ~ decimal.? ~ exp.?

      slice(number) map (d => JNumber(d.toDouble))
    }

    val jstring: Parser[JString] = stringLit map JString.apply

    val jbool:   Parser[JBool]   = (symbol("true", JBool(true)) | symbol("false", JBool(false)))

    def collection[A](left: Char, member: => Parser[A], right: Char): Parser[List[A]] = {
      val members: Parser[List[A]] = (member ~ (token(',') ~ member).many) map { x =>
        val (head, tail: List[(String, A)]) = x
	head :: (tail map (_._2))
      }

      (token(left) ~ members ~ token(right)) map (_._1._2)
    }

    def value :  Parser[JSON]    = jnull | jnumber | jstring | jbool | jarray | jobject

    def jobject: Parser[JObject] = {
      val prop: Parser[(String, JSON)] = (stringLit ~ token(':')).map(_._1) ~ value

      collection('{', prop, '}') map (props => JObject(props.toMap))
    }

    def jarray:  Parser[JArray]  =
      collection('[', value, ']') map (list => JArray(list.toIndexedSeq))

    jobject | jarray
  }
}
