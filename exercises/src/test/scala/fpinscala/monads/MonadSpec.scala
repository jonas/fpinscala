package fpinscala.monads

import org.specs2.mutable.Specification
import org.specs2.matcher.{Matcher,Matchers,TerminationMatchers}
import org.specs2.matcher.Expectable
import org.specs2.matcher.MatchResult
import org.specs2.specification.Scope

import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import fpinscala.parallelism.ThreadPoolContext
import fpinscala.state._
import fpinscala.testing.{Result => _, _}
import fpinscala.parsing.ParseError
import fpinscala.parsing.KParsers
import fpinscala.parsing.KParsers.Parser


class MonadSpec extends Specification with Matchers {

  import Monad._

  "Exercise 11.1" p

  implicit object GenMonadScope extends IntMonadScopeWithResult[Gen, Option] {
    def create(i: Int) = Gen.choose(i, i + 100)
    def run(ma: Gen[Int]) = Some(ma.sample.run(RNG.Simple(42))._1)
    def runList(ma: Gen[List[Int]]) = Some(ma.sample.run(RNG.Simple(42))._1)
  }

  implicit object ParMonadScope extends IntMonadScopeWithResult[Par, Option] with ThreadPoolContext {
    def create(i: Int) = Par.lazyUnit(i)
    def run(ma: Par[Int]) = Some(Par.run(pool)(ma).get)
    def runList(ma: Par[List[Int]]) = Some(Par.run(pool)(ma).get)
  }

  type ErrorOr[A] = Either[ParseError, A]

  implicit object ParserMonadScope extends MonadScope[String, Parser, ErrorOr] {
    val gen = Gen.string('a', 'a')
    def create(s: String) = KParsers.string(s)
    def run(ma: Parser[String]) = KParsers.run(ma)("aaaaaaaaaaa")
    def runList(ma: Parser[List[String]]) = KParsers.run(ma)("aaaaaaaa")
  }

  implicit object OptionMonadScope extends IntMonadScope[Option] {
    def create(i: Int) = if (i < 0) None else Some(i)
  }

  implicit object StreamMonadScope extends IntMonadScope[Stream] {
    def create(i: Int) = Stream.from(i) take 50
  }

  implicit object ListMonadScope extends IntMonadScope[List] {
    def create(i: Int) = if (i < 0) Nil else (0 to i) toList
  }

  implicit object IdMonadScope extends IntMonadScope[Id] {
    def create(i: Int) = Id(i)
  }

  MonadSpecs("genMonad", genMonad)
  MonadSpecs("parMonad", parMonad)
  MonadSpecs("parserMonad", parserMonad(KParsers))
  MonadSpecs("optionMonad", optionMonad)
  MonadSpecs("streamMonad", streamMonad)
  MonadSpecs("listMonad", listMonad)
  MonadSpecs("idMonad", idMonad)

  /* ========================== */

  trait MonadScope[A, M[A], R[A]] {
    def create(a: A): M[A]
    def run(ma: M[A]): R[A]
    def runList(ma: M[List[A]]): R[List[A]]
    def gen: Gen[A]
  }

  trait IntMonadScopeWithResult[M[Int], R[Int]] extends MonadScope[Int, M, R] {
    val gen = Gen.choose(-100, 100)
  }

  trait IntMonadScope[M[Int]] extends IntMonadScopeWithResult[M, M] {
    def run(ma: M[Int]) = ma
    def runList(ma: M[List[Int]]) = ma
  }

  def MonadSpecs[A, M[A], R[A]](name: String, monad: Monad[M])(implicit scope: MonadScope[A, M, R]): Unit = {
    type MonadFn = A => M[A]
    name should {
      "adhere to the monad laws" in {
        monadLaws(monad, scope) must pass
      }

      "implement sequence" in {
        Prop.forAll {
          for { n <- Gen.choose(0, 10); l <- Gen.listOfN(n, scope.gen) } yield l
        } {
          case l =>
	    scope.runList(monad.sequence(l.map(monad.unit(_)))) === scope.runList(monad.unit(l))
        } must pass
      }
    }
  }

  def monadLaws[A, M[A], R[A]](m: Monad[M], scope: MonadScope[A, M, R]): Prop = {
    val ga = scope.gen
    def fnGen =
      for (a <- ga)
        yield new Function1[A, M[A]] {
          def apply(a: A) = scope.create(a)
          override def toString = s"i => Gen.choose($a, i)"
        }

    def equals(a: A, ma: Function1[A, M[A]], mb: Function1[A, M[A]]): Boolean = 
      scope.run(ma(a)) == scope.run(mb(a))

    val associativeLaw =
      Prop.forAll {
        for { i <- ga; a <- fnGen; b <- fnGen; c <- fnGen } yield (i, a, b, c)
      } {
        case (i, a, b, c) =>
	  equals(i, m.compose(m.compose(a, b), c), m.compose(a, m.compose(b, c)))
      }

    val identityLaw =
      Prop.forAll {
        for { i <- ga; a <- fnGen } yield (i, a)
      } {
        case (i, a) =>
          equals(i, m.compose((a: A) => m.unit(a), a), m.compose(a, (b: A) => m.unit(b)))
      }

    associativeLaw && identityLaw
  }


  type MonadLaw = Prop

  case class MonadLawMatcher() extends Matcher[MonadLaw] {
    val testCases = 100
    val maxSize = 100
    val rng = RNG.Simple(System.currentTimeMillis)
 
    def apply[S <: MonadLaw](e: Expectable[S]) = {
      e.value.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) =>
          result(false,
            s"Passed $testCases tests",
            s"Falsified after $n passed tests:\n ``$msg''", e)
        case _ =>
          result(true,
            s"Passed $testCases tests",
            s"Falsified after ? passed tests", e)
      }
    }
  }
 
  def pass = MonadLawMatcher()

}
