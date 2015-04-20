package fpinscala.parsing

import scala.util.matching.Regex

object KParser {
  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] =
      this match {
        case Failure(e, committed) => Failure(f(e), committed)
        case _ => this
      }

    def uncommit: Result[A] =
      this match {
        case Failure(e, true) => Failure(e, false)
        case _ => this
      }

    def addCommit(isCommitted: Boolean): Result[A] =
      this match {
        case Failure(e,c) => Failure(e, c || isCommitted)
        case _ => this
      }

    def advanceSuccess(n: Int): Result[A] =
      this match {
        case Success(a, m) => Success(a, m + n)
        case _ => this
      }
  }
  case class Success[A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]
}

import KParser._

case class KParser[+A](run: Location => Result[A])

object KParsers extends Parsers[KParser] {

  type Parser[A] = KParser[A]

  def run[A](p: Parser[A])(input: String): Either[ParseError,A] =
    p.run(Location(input)) match {
        case Success(a, chars) => Right(a)
        case Failure(error, _) => Left(error)
    }

  def flatMap[A,B](p: Parser[A])(g: A => Parser[B]): Parser[B] = KParser {
    loc => p.run(loc) match {
      case Success(a, n) => g(a).run(loc.advanceBy(n))
                                .addCommit(n != 0)
                                .advanceSuccess(n)
      case e: Failure => e
    }
  }

  def succeed[A](a: A): Parser[A] = KParser {
    loc => Success(a, 0)
  }

  private def slicedString(loc: Location, chars: Int): String =
    loc.input.substring(loc.offset, loc.offset + chars)

  def slice[A](p: Parser[A]): Parser[String] = KParser {
    loc => {
      p.run(loc) match {
        case Success(a, chars) =>
	  if (chars > 0)
	    Success(slicedString(loc, chars), chars)
	  else
	    Success("", chars)
        case e: Failure => e
      }
    }
  }

  def regex(r: Regex): Parser[String] = KParser {
    loc => {
      r.findPrefixOf(loc.input.substring(loc.offset)) match {
        case Some(matched) => Success(matched, matched.length)
        case None          => Failure(loc.toError(r.toString), false)
      }
    }
  }

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = KParser {
    loc => {
      s1.run(loc) match {
        case Failure(error, false) => s2.run(loc)
        case r => r
      }
    }
  }

  def string(s: String): Parser[String] = KParser {
    loc => {
      if (loc.input.startsWith(s, loc.offset))
        Success(s, s.length)
      else
        Failure(loc.toError(s), false)
    }
  }

  def attempt[A](p: Parser[A]): Parser[A] = KParser {
    loc => p.run(loc).uncommit
  }

  def scope[A](msg: String)(p: Parser[A]): Parser[A] = KParser {
    loc => p.run(loc).mapError(_.push(loc, msg))
  }

  def label[A](msg: String)(p: Parser[A]): Parser[A] = KParser {
    loc => p.run(loc).mapError(_.label(msg))
  }

}
