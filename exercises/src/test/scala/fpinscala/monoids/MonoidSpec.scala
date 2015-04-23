package fpinscala.monoids

import org.specs2.mutable.Specification
import org.specs2.matcher.{Matcher,Matchers,TerminationMatchers}
import org.specs2.matcher.Expectable
import org.specs2.matcher.MatchResult

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.ThreadPoolContext
import fpinscala.state._
import fpinscala.testing._

class MonoidSpec extends Specification with Matchers {

  import Monoid._

  "Exercise 10.1" p

  "intAddition monoid" should {
    "adhere to the monoid laws" in {
      monoidLaws(intAddition, Gen.choose(-100, 100)) must pass
    }
  }

  "intMultiplication monoid" should {
    "adhere to the monoid laws" in {
      monoidLaws(intMultiplication, Gen.choose(-100, 100)) must pass
    }
  }

  "booleanOr monoid" should {
    "adhere to the monoid laws" in {
      monoidLaws(booleanOr, Gen.boolean) must pass
    }
  }

  "booleanAnd monoid" should {
    "adhere to the monoid laws" in {
      monoidLaws(booleanAnd, Gen.boolean) must pass
    }
  }

  "Exercise 10.2" p

  "optionMonoid" should {
    "adhere to the monoid laws" in {
      val optionGen: Gen[Option[String]] =
        for { b <- Gen.boolean; s <- Gen.string() }
	  yield if (b) Some(s) else None

      monoidLaws(optionMonoid: Monoid[Option[String]], optionGen) must pass
    }
  }

  "Exercise 10.3" p

  "endoMonoid" should {
    "adhere to the monoid laws" in {
      val endoGen: Gen[Int => Int] =
        for { i <- Gen.choose(-100, 100) }
          yield new Function1[Int, Int] {
            def apply(x: Int) = x + i
            override def toString = s"x + $i"
          }

      val m: Monoid[Int => Int] = endoMonoid

      // monoidLaws(endoMonoid: Monoid[Int => Int], endoGen) must pass
      Prop.forAll {
        for (a <- endoGen; b <- endoGen; c <- endoGen; int <- Gen.choose(-100, 100))
          yield (a, b, c, int)
      } {
        case (a, b, c, int) => m.op(m.op(a, b), c)(int) == m.op(a, m.op(b, c))(int)
      } &&
      Prop.forAll {
        for (a <- endoGen; int <- Gen.choose(-100, 100)) yield (a, int)
      }{
        case (a, int) => m.op(m.zero, a)(int) == m.op(a, m.zero)(int)
      } must pass
    }
  }

  "Exercise 10.5" p

  "foldMap" should {
    "work for stringMonoid with List[Char]" in {
      Prop.forAll(Gen.string() listOfN Gen.choose(0, 10)) { s =>
        foldMap(s, stringMonoid)(_.toString) == s.mkString
      } must pass
    }

    "work for listMonoid with Array[Int]" in {
      val intArrayGen: Gen[Array[Int]] =
        Gen.choose(-10, 10).listOfN(Gen.choose(0, 100)) map (_.toArray)

      Prop.forAll(intArrayGen listOfN Gen.choose(0, 10)) { arrays =>
        foldMap(arrays, listMonoid:Monoid[List[Int]])(_.toList) == arrays.flatten
      } must pass
    }
  }

  "Exercise 10.6" p

  "foldMap" should {
    "permit to implement foldRight" in {
      Prop.forAll(Gen.choose(-100, 100) listOfN Gen.choose(0, 10)) { l =>
        foldRight(l)(0)(_ + _) == l.foldRight(0)(_ + _)
      } must pass
    }

    "permit to implement foldLeft" in {
      Prop.forAll(Gen.choose(-100, 100) listOfN Gen.choose(0, 10)) { l =>
        foldLeft(l)(0)(_ + _) == l.foldLeft(0)(_ + _)
      } must pass
    }
  }

  "Exercise 10.7" p

  "foldMapV" should {
    "work for stringMonoid with List[Char]" in {
      Prop.forAll(Gen.string() listOfN Gen.choose(0, 10) map (_.toIndexedSeq)) { s =>
        foldMapV(s, stringMonoid)(_.toString) == s.mkString
      } must pass
    }

    "work for listMonoid with Array[Int]" in {
      val intArrayGen: Gen[Array[Int]] =
        Gen.choose(-10, 10) listOfN Gen.choose(0, 100) map (_.toArray)

      Prop.forAll(intArrayGen listOfN Gen.choose(0, 10) map (_.toIndexedSeq)) { arrays =>
        foldMapV(arrays, listMonoid:Monoid[List[Int]])(_.toList) == arrays.flatten
      } must pass
    }

    "work for intAddition monoid" in {
      Prop.forAll(Gen.choose(-100, 100) listOfN Gen.choose(0, 10) map (_.toIndexedSeq)) { ints =>
        foldMapV(ints, intAddition)(identity) == ints.sum
      } must pass
    }
  }

  "Exercise 10.8" p

  "parFoldMap" should {
    "sum a sequence of ints with the intAddition monoid" in new ThreadPoolContext {
      val range = 1 to 1000 toIndexedSeq

      Par.run(pool)(parFoldMap(range, intAddition)(identity)) === 500500
      threadCount must be_>=(1)
    }

    "append a sequence of string with the stringMonoid" in new ThreadPoolContext {
      val range = 1 to 100 toIndexedSeq

      Par.run(pool)(parFoldMap(range, stringMonoid)(_.toString)) === (
        "123456789101112131415161718192021222324252627282930313233343536373839" +
	"404142434445464748495051525354555657585960616263646566676869" +
	"707172737475767778798081828384858687888990919293949596979899100"
      )
      threadCount must be_>=(1)
    }
  }

  "Exercise 10.9" p

  "ordered" should {
    "succeed for ordered list of integers" in {
      ordered(1 to 10 toIndexedSeq) === true
      ordered(Vector(1, 2, 3, 5, 8, 13, 21, 34, 55)) === true

      Prop.forAll(Gen.choose(-100, 100) listOfN Gen.choose(0, 100) map (_.toIndexedSeq)) { ints =>
        ordered(ints.sorted) == true
      } must pass
    }

    "fail for unordered list of integers" in {
      ordered(Vector(1, 20, 4, 55, 100)) === false
      def isSorted(ints: IndexedSeq[Int]): Boolean =
        if (ints.length <= 1)
	  true
	else
	  ints(0) < ints(1) && isSorted(ints.tail)

      Prop.forAll(Gen.choose(-100, 100) listOfN Gen.choose(0, 100) map (_.toIndexedSeq)) { ints =>
        if (isSorted(ints)) true else ordered(ints) == false
      } must pass
    }
  }

  type MonoidLaw = Prop

  case class MonoidLawMatcher() extends Matcher[MonoidLaw] {
    val testCases = 100
    val maxSize = 100
    val rng = RNG.Simple(System.currentTimeMillis)
 
    def apply[S <: MonoidLaw](e: Expectable[S]) = {
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
 
  def pass = MonoidLawMatcher()

}
