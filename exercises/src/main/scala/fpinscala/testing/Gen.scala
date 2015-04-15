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

trait Prop {
  def check: Boolean

  def &&(that: Prop): Prop =
    if (this.check)
      that
    else
      new Prop { val check = false }
}

// Implementation of case class Gen - {{{
package gen_case_class_impl {
  case class Gen[+A](sample: State[RNG,A]) {

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(State(s => {
        val (a, s1) = sample.run(s)
        f(a).sample.run(s1)
      }))

    def flatMapViaStateFlatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(x => f(x).sample))

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

  }

  object Gen {

    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      require(start < stopExclusive)
      val interval = stopExclusive - start

      Gen(State(
        RNG.map(RNG.nonNegativeInt)(i => (i % interval) + start)
      ))
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

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      boolean.flatMap(x => if (x) g1 else g2)

    def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
      val median = g1._2 / (g1._2 + g2._2)
      Gen(State(RNG.double)).flatMap(d => if (d <= median) g1._1 else g2._1)
    }

  }
}
// }}}
object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???
}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

