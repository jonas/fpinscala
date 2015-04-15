package fpinscala.testing

import java.util.concurrent._
import scala.concurrent.duration._
import scala.collection.immutable
//import scala.collection.immutable.{Stream => StdStream}

import org.specs2.mutable.Specification
import org.specs2.matcher.{Matcher,Matchers,TerminationMatchers}
import org.specs2.ScalaCheck

import fpinscala.parallelism._
import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par

class GenSpec extends Specification with Matchers with ScalaCheck {

  "Exercise 8.1" p

  """
  What properties specify the implementation of a sum: List[Int] => Int?

  􏰀- Reversing a list and summing it should give the same result as
     summing the original, nonreversed list.
   - If all elements of the list are the same value, the sum should
     equal any element times the size of the list.
   - The sum of an empty list is zero.
  """ txt

  "Exercise 8.2" p

  """
  What properties specify a function that finds the maximum of a List[Int]?

  􏰀- Reversing a list and finding the maximum should give the same
     result as finding the maximum of the original, nonreversed list.
   - If all elements of the list are the same value, the maximum should
     equal any element of the list.
   - The maximum of an empty list will generate an error.
  """ txt

  "Exercise 8.3" p

  "Prop.&&" should {
    case object No extends Prop {
      val check = false
    }

    case object Yes extends Prop {
      val check = true
    }

    "Succeed if both props check" in {
      (Yes && Yes).check === true
      (Yes && Yes && Yes).check === true
    }

    "Fail if either prop does not check" in {
      (No  && Yes).check === false
      (Yes && No ).check === false 
      (No  && No ).check === false
    }
  }

  // Test helper
  def streamOf[A](gen: gen_case_class_impl.Gen[A], seed: Int, size: Int = 10): immutable.Stream[A] = {
    def stream(rng: RNG): immutable.Stream[A] = {
        val (a, rng2) = gen.sample.run(rng)
        immutable.Stream.cons(a, stream(rng2))
      }

    stream(RNG.Simple(seed))
  }

  "Exercise 8.4" p

  "Gen.choose" should {
    import gen_case_class_impl._

    "generate integers in range" >> prop { (seed: Int, start: Int, end: Int) =>
      (start < end) ==> {
        val gen = Gen.choose(start, end)

        streamOf(gen, seed) take(100) must contain(beBetween(start, end).excludingEnd).forall
      }
    }
  }

  "Exercise 8.5" p

  "Gen.unit" should {
    import gen_case_class_impl._

    "generate the same value when given a pure function" >> prop { (seed: Int) =>
      val gen = Gen.unit(42)

      streamOf(gen, seed) take(100) must contain(===(42)).forall
    }
  }

  "Gen.boolean" should {
    import gen_case_class_impl._

    "generate boolean values" >> prop { (seed: Int) =>
      val gen = Gen.boolean

      streamOf(gen, seed) take(100) must contain(anyOf(true, false))
    }
  }

  "Gen.listOfN" should {
    import gen_case_class_impl._

    "generate a list of ints" >> prop { (seed: Int, n: Int, start: Int, end: Int) =>
      (start < end && n < 1024) ==> {
        val gen = Gen.listOfN(n, Gen.choose(start, end))

        streamOf(gen, seed) take(100) must contain((list: List[Int]) =>
          list must contain(beBetween(start, end).excludingEnd).forall
	)
      }
    }

    "generate a list of booleans" >> prop { (seed: Int, nSeed: Int) => {
        val n = math.abs(nSeed) % 1024 + 1
        val gen = Gen.listOfN(n, Gen.boolean)

        streamOf(gen, seed) take(100) must contain((list: List[Boolean]) =>
          list must contain(anyOf(true, false))
        )
      }
    }
  }

  "Exercise 8.6" p

  "Gen.flatMap" should {
    import gen_case_class_impl._

    "allow to create a new string generator based on Gen.choose" >> prop { (seed: Int, start: Int, end: Int) =>
      (start < end) ==> {
        val gen = Gen.choose(start, end).flatMap(i => Gen.unit(i.toString))

        streamOf(gen, seed) take(100) must contain(beMatching("-?[0-9]+"))
      }
    }

    "allow to create a new string generator based on Gen.boolean" >> prop { (seed: Int) =>
      val gen = Gen.boolean.flatMap(b => Gen.unit(b.toString))

      streamOf(gen, seed) take(100) must contain(anyOf("true", "false"))
    }

    "allow to create a new case class generator based on Gen.boolean" >> prop { (seed: Int) =>
      sealed trait CoinSide
      case object Heads extends CoinSide
      case object Tails extends CoinSide
      val gen = Gen.boolean.flatMap(b => Gen.unit(if (b) Heads else Tails))

      streamOf(gen, seed) take(100) must contain(anyOf(Heads, Tails))
    }

    "be used to implement Gen.listOfN" >> prop { (seed: Int, start: Int, end: Int, sizeSeed: Int) =>
      (start < end) ==> {
        val size = math.abs(sizeSeed) % 1024 + 1
	val sizeGen = Gen.choose(0, size)
        val gen = Gen.choose(start, end).listOfN(sizeGen)

        streamOf(gen, seed) take(100) must contain((list: List[Int]) =>
          list must contain(beBetween(start, end).excludingEnd).forall
	)
      }
    }
  }

  "Exercise 8.7" p

  "Gen.union" should {
    import gen_case_class_impl._

    "allow to combine two int generators" >> prop { (seed: Int) =>
      val gen = Gen.union(Gen.unit(42), Gen.unit(-1))

      streamOf(gen, seed) take(100) must contain(anyOf(42, -1))
    }

    "allow to combine two generators" >> prop { (seed: Int) =>
      var i = 0
      val ints = List(1, 2, 3)
      def evilInts: Int = { i = (i + 1) % ints.size; ints(i) }
      val gen = Gen.union(Gen.unit(evilInts), Gen.boolean)

      streamOf(gen, seed) take(100) must contain(anyOf(1, 2, 3, true, false))
    }
  }

  "Exercise 8.8" p

  "Gen.weighted" should {
    import gen_case_class_impl._

    "generate values with probability proportional to the weights" >> prop { (seed: Int) =>
      val gen = Gen.weighted((Gen.unit("yay"), 0.75), (Gen.unit("nay"), 0.25))

      val list = streamOf(gen, seed) take(1000) toList
      val yayProb = list.count(_ == "yay").toDouble / list.size
      val nayProb = list.count(_ == "nay").toDouble / list.size

      yayProb must beBetween(0.70, 0.80)
      nayProb must beBetween(0.20, 0.30)
    }

    "generate from only one side if the weight is zero" >> prop { (seed: Int) =>
      val gen = Gen.weighted((Gen.unit("yay"), 1.0), (Gen.unit("nay"), 0.0))

      val list = streamOf(gen, seed) take(1000) toList
      val yayProb = list.count(_ == "yay").toDouble / list.size
      val nayProb = list.count(_ == "nay").toDouble / list.size

      yayProb must beBetween(0.99, 1.0)
      nayProb must beBetween(0.0, 0.01)
    }
  }
}
