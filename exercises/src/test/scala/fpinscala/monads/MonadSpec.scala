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
    def genFrom = i => Gen.choose(i, i + 100)
    def run[B] = ma => Some(ma.sample.run(RNG.Simple(42))._1)
  }

  implicit object ParMonadScope extends IntMonadScopeWithResult[Par, Option] with ThreadPoolContext {
    def genFrom = i => Par.lazyUnit(i)
    def run[B] = ma => Some(Par.run(pool)(ma).get)
  }

  type ErrorOr[A] = Either[ParseError, A]

  implicit object ParserMonadScope extends MonadScope[String, Parser, ErrorOr] {
    val gen = Gen.string('a', 'a')
    def genFrom = s => KParsers.string(s)
    def run[B] = ma => KParsers.run(ma)("aaaaaaaaaaaaaaaaaaaaaaaa")
  }

  implicit object RNGStateMonadScope extends IntMonadScopeWithResult[RNGState, Option] {
    def genFrom = i => State.unit(i)
    def run[B] = ma => Some(ma.run(RNG.Simple(42))._1)
  }

  implicit object OptionMonadScope extends IntMonadScope[Option] {
    def genFrom = i => if (i < 0) None else Some(i)
  }

  implicit object StreamMonadScope extends IntMonadScope[Stream] {
    def genFrom = i => Stream.from(i) take 50
  }

  implicit object ListMonadScope extends IntMonadScope[List] {
    def genFrom = i => if (i < 0) Nil else (0 to i) toList
  }

  implicit object IdMonadScope extends IntMonadScope[Id] {
    def genFrom = i => Id(i)
  }

  MonadSpecs("genMonad", genMonad)
  MonadSpecs("parMonad", parMonad)
  MonadSpecs("parserMonad", parserMonad(KParsers))
  MonadSpecs("optionMonad", optionMonad)
  MonadSpecs("streamMonad", streamMonad)
  MonadSpecs("listMonad", listMonad)
  MonadSpecs("idMonad", idMonad)
  MonadSpecs("stateMonad", stateMonad)

  type IntReader[X] = Reader[Int, X]

  implicit object ReaderMonadScope extends IntMonadScopeWithResult[IntReader, Option] {
    def genFrom = i => Reader(_ => i)
    def run[B] = ma => Some(ma.run(42))
  }

  MonadSpecs[Int, IntReader, Option]("readerMonad", readerMonad[Int])

  "readerMonad" should {
    "allow composition" in {
      val three = readerMonad[Int].unit(3)
      val triple = readerMonad[Int].map(three)(_ * 3)
      val thricePlus2 = readerMonad.map(triple)(_ + 2)

      three.run(42) == 3 &&
        triple.run(3) === 9 &&
        thricePlus2.run(3) === 11
    }
  }

  /* ========================== */

  trait MonadScope[A, M[_], R[_]] {
    def run[B]: M[B] => R[B]
    def gen: Gen[A]
    def genFrom: A => M[A]
  }

  trait IntMonadScope[M[_]] extends IntMonadScopeWithResult[M, M] {
    def run[B] = identity
  }

  trait IntMonadScopeWithResult[M[Int], R[Int]] extends MonadScope[Int, M, R] {
    val gen = Gen.choose(-100, 100)
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
	    scope.run(monad.sequence(l.map(monad.unit(_)))) === scope.run(monad.unit(l))
        } must pass
      }

      "implement traverse" in {
        Prop.forAll {
          for { n <- Gen.choose(0, 10); l <- Gen.listOfN(n, scope.gen) } yield l
        } {
          case l =>
            scope.run(monad.traverse(l)(monad.unit(_))) === scope.run(monad.unit(l))
        } must pass
      }

      "implement replicateM" in {
        Prop.forAll {
          for { n <- Gen.choose(0, 10); a <- scope.gen } yield (n, a)
        } {
          case (n, a) =>
            val l: List[A] = (for (i <- 0 until n) yield a) toList

            scope.run(monad.replicateM(n, monad.unit(a))) === scope.run(monad.unit(l))
        } must pass
      }

      "implement filterM" in {
        val filterHead =
          Prop.forAll {
            for { n <- Gen.choose(0, 10); l <- Gen.listOfN(n, scope.gen) } yield l
          } {
            case l =>
              def noHead(a: A) = Some(a) == l.headOption

              scope.run(monad.filterM(l)(a => monad.unit(noHead(a)))) ===
                scope.run(monad.unit(l.filter(noHead)))
          }

        val filterAll =
          Prop.forAll {
            for { n <- Gen.choose(0, 10); l <- Gen.listOfN(n, scope.gen) } yield l
          } {
            case l =>
              scope.run(monad.filterM(l)(_ => monad.unit(true))) === scope.run(monad.unit(l)) &&
              scope.run(monad.filterM(l)(_ => monad.unit(false))) === scope.run(monad.unit(Nil))
          }

        (filterHead && filterAll) must pass
      }

      "implement compose" in {
        Prop.forAll {
          for { a <- scope.gen } yield a
        } {
          case a =>
            scope.run(monad.compose(
              (a: A)      => monad.unit(a.toString),
              (b: String) => monad.unit(b.length))(a)) === scope.run(monad.unit(a.toString.length))
        } must pass
      }

      "implement flatMap in terms of compose" in {
        Prop.forAll {
          for { a <- scope.gen } yield monad.unit(a)
        } {
          case ma =>
            scope.run(monad.flatMap(ma)(x => monad.unit(x))) === scope.run(ma)
        } must pass
      }

      "implement join in terms of flatMap" in {
        Prop.forAll {
          for { a <- scope.gen } yield monad.unit(a)
        } {
          case ma =>
            scope.run(monad.join(monad.unit(ma))) === scope.run(ma)
        } must pass
      }
    }
  }

  def monadLaws[A, M[A], R[A]](m: Monad[M], scope: MonadScope[A, M, R]): Prop = {
    val ga = scope.gen
    def fnGen =
      for (a <- ga)
        yield new Function1[A, M[A]] {
          def apply(a: A) = scope.genFrom(a)
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
