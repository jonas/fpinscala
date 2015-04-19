package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  /*
   * Primitives
   */
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def succeed[A](a: A): Parser[A]
  def slice[A](p: Parser[A]): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  def attempt[A](p: Parser[A]): Parser[A]

  /*
   * Combinators and helpers
   */

  def map[A,B](pa: Parser[A])(f: A => B): Parser[B] =
    flatMap(pa)(a => succeed(f(a)))

  def defaultUnit[A](a: A): Parser[A] =
    string("") map (_ => a)

  def map2[A,B,C](pa: Parser[A], pb: => Parser[B])(f: (A,B) => C): Parser[C] =
    flatMap(pa)(a => map(pb)(b => f(a, b)))

  def product[A,B](pa: Parser[A], pb: => Parser[B]): Parser[(A,B)] =
    flatMap(pa)(a => map(pb)((a, _)))

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def optional[A](p: Parser[A]): Parser[Option[A]] =
    or(map(p)(a => Some(a)), succeed(None))

  def many[A](p: Parser[A]): Parser[List[A]] =
    or(map2(p, many(p))(_ :: _), succeed(Nil))

  def many1[A](p: Parser[A]) =
    map2(p, many(p))(_ :: _)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0)
      succeed(Nil)
    else
      map2(p, listOfN(n - 1, p))(_ :: _)

  def as[A,B](p: Parser[A], b: B): Parser[B] =
    p map (_ => b)

  def sep[A](p: Parser[A], s: String): Parser[List[A]] = {
    def extract(r: (A, List[(String, A)])): List[A] = {
      val (head, tail) = r
      head :: (tail map (_._2))
    }

    (p ~ (s ~ p).many) map (extract(_))
  }

  def surround[A](left: String, right: String)(p: => Parser[A]): Parser[A] =
    (left ~ p ~ right) map (_._1._2)

  case class ParserOps[A](p: Parser[A]) {
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def ~[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p,p2)
    def **[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p,p2)
    def product[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p,p2)
    def ?[B>:A](): Parser[Option[B]] = self.optional(p)
    def *[B>:A](): Parser[List[B]] = self.many(p)
    def many[B>:A]: Parser[List[B]] = self.many(p)
    def +[B>:A](): Parser[List[B]] = self.many1(p)
    def slice: Parser[String] = self.slice(p)
    def as[B](b: B): Parser[B] = self.as(p, b)
    def sep(s: String): Parser[List[A]] = self.sep(p, s)
  }

  object Laws { self =>

    def equals[A](p1: Parser[A], p2: Parser[A])(input: Gen[String]): Prop =
      forAll(input)(input => run(p1)(input) == run(p2)(input))

    def prequals[A](p1: Parser[A], p2: Parser[A])(input: Gen[String]): Prop =
      forAll(input)(input => (run(slice(p1))(input), run(slice(p2))(input)) match {
        case (Left(_), Left(_)) => true
        case (r1,      r2)      => r1 == r2
      })

    def means[A](p1: Parser[A], p2: Parser[A])(input: Gen[String]): Prop =
      forAll(input)(input => if (run(p1)(input).isRight) run(p2)(input).isRight else true)

    def equalsEither[A](p: Parser[A], r: Either[ParseError,A])(input: Gen[String]): Prop =
      forAll(input)(input => run(p)(input) == r)

    def equalsMapped[A,B](p: Parser[A], f: String => B)(input: Gen[String]): Prop =
      forAll(input)(input => run(p)(input) match {
        case Right(a) => a == f(input)
        case _        => true /* ignored! */
      })

    case class EvalOps[A](p: Parser[A]) {
      def ===[B>:A](p2: => Parser[B]) = self.equals(p, p2) _
      def =~=[B>:A](p2: => Parser[B]) = self.prequals(p, p2) _
      def =*=[B](f: String => B) = self.equalsMapped(p, f) _
      def ===[B>:A](p2: Either[ParseError,B]) = self.equalsEither(p, p2) _
      def ==>[B>:A](p2: => Parser[B]) = self.means(p, p2) _
    }

    implicit def evaluator[A](p: Parser[A]) = EvalOps[A](p)
    implicit def right[A](a: A) = Right(a)

    type Law = Gen[String] => Prop

    def mapIdentity[A](p: Parser[A]) =
      map(p)(a => a) === p

    def succeedLaw[A](a: A) =
      succeed(a) === a

    def orIsAssociative[A](a: Parser[A], b: Parser[A], c: Parser[A]) =
      ((a | b) | c) === (a | (b | c))

    def orIsCommutative[A](a: Parser[A], b: Parser[A]) =
      (a | b) =~= (b | a)

    // Exercise 9.2
    def productIsAssociative[A](a: Parser[A], b: Parser[A], c: Parser[A]) =
      ((a ~ b) ~ c) =~= (a ~ (b ~ c))

    def productDistributesOverOr[A](a: Parser[A], b: Parser[A], c: Parser[A]) =
      (a ~ (b | c)) === (attempt(a ~ b) | attempt(a ~ c))

    def many1MeansManyMatches[A](a: Parser[A]) =
      (a +) ==> (a *)

    def optionalMeansManyMatches[A](a: Parser[A]) =
      (a ?) ==> (a *)

    /** Spec **/
    def manyMapSize[A](p: Parser[A]) =
      (p *).map(_.size) =*= (_.size)

    def manyMapSize2[A](p: Parser[A]) =
      (p *).slice.map(_.size) =*= (_.size)

    def sliceLaw[A](p1: Parser[A], p2: Parser[A]) =
      slice((p1 | p2) *) =*= (identity)

    def lengthEncodedStringParser =
      slice("[0-9]+".r flatMap (length => listOfN(length.toInt, "a" | "b"))) =*= (identity)

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
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc,msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_,s)).toList)

  def latest: Option[(Location,String)] =
    stack.lastOption

  def latestLoc: Option[Location] =
    latest map (_._1)
}

