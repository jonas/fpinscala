package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

package prop_trait {
  trait Prop {
    def check: Boolean

    def &&(that: Prop): Prop =
      new Prop { def check = Prop.this.check && that.check }
  }
}

// Implementation of case class Gen - {{{
case class Gen[+A](sample: State[RNG,A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(x => f(x).sample))

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => {
      @scala.annotation.tailrec
      def go(count: Int, l: List[A])(rng: RNG): (List[A], RNG) = 
        if (count <= 0)
          (l, rng)
        else {
          val (a, rng2) = sample.run(rng)
          go(count - 1, a :: l)(rng2)
        }

      Gen(State(go(n, Nil)))
    })

  def listOfN2(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN2(n, this))

  def unsized: SGen[A] =
    SGen(_ => this)

}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    require(start < stopExclusive)
    val interval = stopExclusive - start

    Gen(State(
      RNG.map(RNG.nonNegativeInt)(i => (i % interval) + start)
    ))
  }

  def choose2(start: Int, stopExclusive: Int): Gen[Int] = {
    require(start < stopExclusive)
    val interval = stopExclusive - start

    Gen(State(RNG.nonNegativeInt).map(i => (i % interval) + start))
  }

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(
      RNG.map(RNG.int)(_ % 2 == 0)
    ))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    @scala.annotation.tailrec
    def go(count: Int, l: List[A])(rng: RNG): (List[A], RNG) = 
      if (count <= 0)
        (l, rng)
      else {
        val (a, rng2) = g.sample.run(rng)
        go(count - 1, a :: l)(rng2)
      }

    Gen(State(go(n, Nil)))
  }

  def listOfN2[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(x => if (x) g1 else g2)

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val median = g1._2 / (g1._2 + g2._2)
    Gen(State(RNG.double)).flatMap(d => if (d <= median) g1._1 else g2._1)
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN2(n, g))
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] =
    forSize(n)

  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(n => forSize(n).flatMap(f))

  def map[B](f: A => B): SGen[B] =
    SGen(n => forSize(n).map(f))
}

// }}}

object Result {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
}

import Result._

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  val isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  val isFalsified = true
}

import Result._

case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Passed =>
        p.run(max, n, rng)
      case result @ Falsified(_, _) =>
        result
    }
  }

  def ||(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case result @ Passed =>
        result
      case Falsified(_, _) =>
        p.run(max, n, rng)
    }
  }
}

object Prop {
  type MaxSize = Int

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop { (max,n,rng) =>
    val casesPerSize = (n + (max - 1)) / max
    val props: Stream[Prop] =
      Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
    val prop: Prop =
      props.map(p => Prop { (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
    prop.run(max,n,rng)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}
