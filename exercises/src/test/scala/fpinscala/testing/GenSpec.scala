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

  val SAMPLES = 100
  def listSize(sizeSeed: Int) = math.abs(sizeSeed % 1024) + 1

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

  "Prop.&& (trait)" should {
    import prop_trait.Prop

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
  def streamOf[A](gen: Gen[A], seed: Int = 42): immutable.Stream[A] = {
    def stream(rng: RNG): immutable.Stream[A] = {
        val (a, rng2) = gen.sample.run(rng)
        immutable.Stream.cons(a, stream(rng2))
      }

    stream(RNG.Simple(seed))
  }

  def oneOf[A](gen: Gen[A], seed: Int): A =
    streamOf(gen, seed) take(1) head

  "Exercise 8.4" p

  "Gen.choose" should {
    "generate integers in range" >> prop { (seed: Int, start: Int, end: Int) =>
      (start < end) ==> {
        val gen = Gen.choose(start, end)

        streamOf(gen, seed) take(SAMPLES) must contain(beBetween(start, end).excludingEnd).forall
      }
    }
  }

  "Exercise 8.5" p

  "Gen.unit" should {
    "generate the same value when given a pure function" >> prop { (seed: Int) =>
      val gen = Gen.unit(42)

      streamOf(gen, seed) take(SAMPLES) must contain(===(42)).forall
    }
  }

  "Gen.boolean" should {
    "generate boolean values" >> prop { (seed: Int) =>
      val gen = Gen.boolean

      streamOf(gen, seed) take(SAMPLES) must contain(anyOf(true, false))
    }
  }

  "Gen.listOfN" should {
    "generate a list of ints" >> prop { (seed: Int, n: Int, start: Int, end: Int) =>
      (start < end && n < 1024) ==> {
        val gen = Gen.listOfN(n, Gen.choose(start, end))

        streamOf(gen, seed) take(SAMPLES) must contain((list: List[Int]) =>
          list must contain(beBetween(start, end).excludingEnd).forall
	)
      }
    }

    "generate a list of booleans" >> prop { (seed: Int, sizeSeed: Int) => {
        val n = listSize(sizeSeed)
        val gen = Gen.listOfN(n, Gen.boolean)

        streamOf(gen, seed) take(SAMPLES) must contain((list: List[Boolean]) =>
          list must contain(anyOf(true, false))
        )
      }
    }
  }

  "Exercise 8.6" p

  "Gen.flatMap" should {
    "allow to create a new string generator based on Gen.choose" >> prop { (seed: Int, start: Int, end: Int) =>
      (start < end) ==> {
        val gen = Gen.choose(start, end).flatMap(i => Gen.unit(i.toString))

        streamOf(gen, seed) take(SAMPLES) must contain(beMatching("-?[0-9]+"))
      }
    }

    "allow to create a new string generator based on Gen.boolean" >> prop { (seed: Int) =>
      val gen = Gen.boolean.flatMap(b => Gen.unit(b.toString))

      streamOf(gen, seed) take(SAMPLES) must contain(anyOf("true", "false"))
    }

    "allow to create a new case class generator based on Gen.boolean" >> prop { (seed: Int) =>
      sealed trait CoinSide
      case object Heads extends CoinSide
      case object Tails extends CoinSide
      val gen = Gen.boolean.flatMap(b => Gen.unit(if (b) Heads else Tails))

      streamOf(gen, seed) take(SAMPLES) must contain(anyOf(Heads, Tails))
    }

    "be used to implement Gen.listOfN" >> prop { (seed: Int, start: Int, end: Int, sizeSeed: Int) =>
      (start < end) ==> {
        val size = listSize(sizeSeed)
        val sizeGen = Gen.choose(0, size)
        val gen = Gen.choose(start, end).listOfN(sizeGen)

        streamOf(gen, seed) take(SAMPLES) must contain((list: List[Int]) =>
          list must contain(beBetween(start, end).excludingEnd).forall
	)
      }
    }
  }

  "Exercise 8.7" p

  "Gen.union" should {
    "allow to combine two int generators" >> prop { (seed: Int) =>
      val gen = Gen.union(Gen.unit(42), Gen.unit(-1))

      streamOf(gen, seed) take(SAMPLES) must contain(anyOf(42, -1))
    }

    "allow to combine two generators" >> prop { (seed: Int) =>
      var i = 0
      val ints = List(1, 2, 3)
      def evilInts: Int = { i = (i + 1) % ints.size; ints(i) }
      val gen = Gen.union(Gen.unit(evilInts), Gen.boolean)

      streamOf(gen, seed) take(SAMPLES) must contain(anyOf(1, 2, 3, true, false))
    }
  }

  "Exercise 8.8" p

  "Gen.weighted" should {
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

  "Exercise 8.9" p

  "Prop.&& and Prop.|| (case class)" should {
    val Yes = Prop((max, tests, rng) => Passed)
    val No  = Prop((max, tests, rng) => Falsified("#2 failed", 0))

    def check(prop: Prop) =
      prop.run(42, 42, RNG.Simple(42))

    "pass for && if all props pass" in {
      check(Yes && Yes).isFalsified === false
      check(Yes && Yes && Yes).isFalsified === false
    }

    "fail for && if any prop is falsified" in {
      check(No  && No ).isFalsified === true
      check(No  && Yes).isFalsified === true
      check(Yes && No ).isFalsified === true 
      check(Yes && Yes && No).isFalsified === true 
    }

    "pass for || if any props pass" in {
      check(Yes || Yes).isFalsified === false
      check(Yes || No ).isFalsified === false
      check(No  || Yes).isFalsified === false
      check(Yes || Yes || Yes).isFalsified === false
      check(Yes || Yes || No).isFalsified === false 
      check(No  || No  || Yes).isFalsified === false
    }

    "fail for || if all props are falsified" in {
      check(No  || No ).isFalsified === true
      check(No  || No  || No).isFalsified === true
      check(No  || No  || No  || No).isFalsified === true
    }
  }

  "Exercise 8.10" p

  "Gen.unsized" should {
    "return the original generator regardless of forSize argument" in {
      List(Gen.unit(42), Gen.boolean, Gen.choose(1, 10)) must contain(
        (gen: Gen[_]) => {
	  val sgen = gen.unsized
          (0 to SAMPLES) must contain(
            (i: Int) => sgen.forSize(i) === gen)
        })
    }
  }

  "Exercise 8.11" p

  "SGen.flatMap" should {
    def generate[A](sgen: SGen[A]): A =
      sgen.forSize(1234).sample.run(RNG.Simple(0))._1

    val f: Int => Char = _.toChar
    val sgen = Gen.unit(42).unsized

    "allow the definition of SGen.map" in {
      def flatMapBasedSGenMap[A,B](sgen: SGen[A])(f: A => B): SGen[B] =
        sgen.flatMap(a => Gen.unit(f(a)))

      generate(flatMapBasedSGenMap(sgen)(f)) === generate(sgen.map(f))
    }

    "allow creation of new types of generators" in {
      generate(sgen.flatMap(`42` => Gen.unit(f(`42`)))) === '*'
    }
  }

  "SGen.map" should {
    "permit creation of new generator based on the original generators" in {
      val sgen = Gen.unit(42).unsized
      val gen = sgen.map(`42` => `42`.toChar).forSize(1234)

      gen.sample.run(RNG.Simple(0))._1 === '*'
    }
  }

  "Exercise 8.12" p

  "Gen.listOf" should {
    "defer size specification to SGen evaluation" >> prop { (seed: Int, start: Int, end: Int, sizeSeed: Int) =>
      (start < end) ==> {
        val size = listSize(sizeSeed)
        val sgen = Gen.listOf(Gen.choose(start, end))

        streamOf(sgen.forSize(size), seed) take(SAMPLES) must contain((list: List[Int]) =>
          list must contain(beBetween(start, end).excludingEnd).forall
	)
      }
    }

    "allow creation of empty lists" in {
       streamOf(Gen.listOf(Gen.boolean).forSize(0)).head must beEmpty
    }

    "fail for the maxProp example in the book" in {
      val smallInt = Gen.choose(-10, 10)
      val maxProp = Prop.forAll(Gen.listOf(smallInt)) { ns =>
        val max = ns.max
        !ns.exists(_ > max)
      }

      maxProp.run(100, 100, RNG.Simple(System.currentTimeMillis)).isFalsified === true
    }
  }

  "Exercise 8.13" p

  "Gen.listOf1" should {
    "defer size specification to SGen evaluation" >> prop { (seed: Int, start: Int, end: Int, sizeSeed: Int) =>
      (start < end) ==> {
        val size = listSize(sizeSeed)
        val sgen = Gen.listOf1(Gen.choose(start, end))

        streamOf(sgen.forSize(size), seed) take(SAMPLES) must contain((list: List[Int]) =>
          list must contain(beBetween(start, end).excludingEnd).forall
	)
      }
    }

    "not allow creation of empty lists" in {
       streamOf(Gen.listOf1(Gen.boolean).forSize(0)).head must not beEmpty
    }

    "work for the maxProp example in the book" in {
      val smallInt = Gen.choose(-10, 10)
      val maxProp = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
        val max = ns.max
        !ns.exists(_ > max)
      }

      maxProp.run(100, 100, RNG.Simple(System.currentTimeMillis)) === Passed
    }
  }

  "Exercise 8.14" p

  "List.sorted property" should {
    implicit class StringPropOps(description: String) {
      def where(prop: Prop): Prop =
        prop.withDescription(description)
    }
    val smallInt = Gen.choose(-10, 10)
    val posInt = Gen.choose(1, 10)
    val sortedProp =
      ("The head element of a sorted list must be <= all elements in the tail." where
        Prop.forAll(Gen.listOf1(smallInt)) { ns =>
          val sorted = ns.sorted
          sorted.tail.forall(sorted.head <= _)
        }
      ) &&
      ("The last element of a sorted list must be >= all elements before it." where
        Prop.forAll(Gen.listOf1(smallInt)) { ns =>
          val sorted = ns.sorted
          sorted.init.forall(sorted.last >= _)
        }
      ) &&
      ("List of zero or one element equals itself when sorted." where
        Prop.forAll(Gen.listOf(smallInt)) { ns =>
          ns.size > 1 || ns.sorted == ns 
        }
      ) &&
      ("List containing the same element equals itself when sorted." where
        Prop.forAll(posInt) { size =>
          val list = List.fill(size)(size)
          list.sorted == list
        }
      )

    "pass" in {
      sortedProp.run(100, 100, RNG.Simple(System.currentTimeMillis)) === Passed
    }
  }

  "Exercise 8.16" p

  "Gen.parInt" should {
    "proove map(y)(x => x) == y" in {
      val p4 = Prop.forAllPar(Gen.parInt(5))(n =>
        Par.equal(Par.map(n)(y => y), n)
      )

      p4.run(100, 100, RNG.Simple(System.currentTimeMillis)) === Passed
    }
  }

  "Exercise 8.17" p

  "Gen.parInt" should {
    "proove fork(x) == x" in {
      val forkProp = Prop.forAllPar(Gen.parInt(5))(n =>
        Par.equal(Par.fork(n), n)
      )

      forkProp.run(100, 100, RNG.Simple(System.currentTimeMillis)) === Passed
    }
  }

  "Exercise 8.18" p

  "List.takeWhile property" should {
    implicit class StringPropOps(description: String) {
      def where(prop: Prop): Prop =
        prop.withDescription(description)
    }
    val smallInt = Gen.choose(-10, 10)
    val posInt = Gen.choose(1, 10)
    val takeWhileProp =
      ("When the predicate is always true, takeWhile should take the whole list." where
        Prop.forAll(Gen.listOf(smallInt)) { ns =>
	  ns.takeWhile(_ => true) == ns
        }
      ) &&
      ("When the predicate is always false, takeWhile should return an empty list." where
        Prop.forAll(Gen.listOf(smallInt)) { ns =>
	  ns.takeWhile(_ => false) == Nil
        }
      ) &&
      ("All elements in the generated list should satisfy the predicate given to takeWhile." where
        Prop.forAll(Gen.listOf1(smallInt)) { ns =>
	  def predicate(i: Int) = i <= ns.head
	  ns.takeWhile(predicate).forall(predicate)
        }
      )

    "pass" in {
      takeWhileProp.run(100, 100, RNG.Simple(System.currentTimeMillis)) === Passed
    }
  }

  "Relationship between List.takeWhile and List.dropWhile" should {
    implicit class StringPropOps(description: String) {
      def where(prop: Prop): Prop =
        prop.withDescription(description)
    }
    val smallInt = Gen.choose(-10, 10)
    val posInt = Gen.choose(1, 10)
    val takeWhileAndDropWhileProp =
      ("The concatenated result of takeWhile and dropWhile should equal the original list." where
        Prop.forAll(Gen.listOf1(smallInt)) { ns =>
	  def predicate(i: Int) = i <= ns.head

	  ns == (ns.takeWhile(predicate) ++ ns.dropWhile(predicate))
        }
      )

    "pass" in {
      takeWhileAndDropWhileProp.run(100, 100, RNG.Simple(System.currentTimeMillis)) === Passed
    }
  }
}
