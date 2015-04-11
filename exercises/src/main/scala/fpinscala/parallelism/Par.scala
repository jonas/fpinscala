package fpinscala.parallelism

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }
  
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
  }
  
  private case class TimedFuture[A,B,C](af: Future[A], bf: Future[B], f: (A,B) => C) extends Future[C] {
    def isDone = af.isDone && bf.isDone
    def isCancelled = af.isCancelled && bf.isCancelled
    def cancel(evenIfRunning: Boolean): Boolean = {
      val aCancelled = af.cancel(evenIfRunning)
      val bCancelled = bf.cancel(evenIfRunning)

      aCancelled && bCancelled
    }
    def get = f(af.get, bf.get)
    def get(timeout: Long, units: TimeUnit) = {
      val startTime = System.nanoTime
      val a = af.get(timeout, units)
      val duration = (System.nanoTime - startTime)
      val remaining = units.toNanos(timeout) - duration
      val b = bf.get(remaining, TimeUnit.NANOSECONDS)

      f(a, b)
    }
  }

  def map2Timed[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)

      TimedFuture(af, bf, f)
    }
  
  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] =
    (a: A) => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(Par.unit(Nil:List[A]))((p, acc) => {
      Par.map2(p, acc)(_ :: _)
    })

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val p = asyncF(f)
    as.foldRight(Par.unit(Nil:List[A]))((a: A, acc) => {
      Par.map2(p(a), acc)((include, t) => if (include) a :: t else t)
    })
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => choices(run(es)(n).get)(es)

  def choiceNChoice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(x => if (x) 0 else 1))(List(t, f))

  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
    es => choices(run(es)(key).get)(es)

  def chooser[A,B](cond: Par[A])(choices: A => Par[B]): Par[B] =
    es => choices(run(es)(cond).get)(es)

  def chooserChoice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(x => if (x) t else f)

  def chooserChoiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(choices(_))

  def chooserChoiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
    chooser(key)(choices(_))

  def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    chooser(p)(f)

  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())

  def flatMapUsingJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(map(p)(f))

  def joinUsingFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(x => x)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  def parSum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(Par.fork(parSum(l)), Par.fork(parSum(r)))(_ + _)
    }

  def parIntOp(ints: IndexedSeq[Int])(f: (Int, Int) => Int): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(Par.fork(parIntOp(l)(f)), Par.fork(parIntOp(r)(f)))(f(_, _))
    }

  def parIntOpSum(ints: IndexedSeq[Int]) =
    parIntOp(ints)(_ + _)

  def mapReduce[A,B](as: IndexedSeq[A])(zero: A, map: A => B, reduce: (B, B) => B): Par[B] =
    if (as.length <= 1)
      Par.asyncF(map)(as.headOption getOrElse zero)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      val lf = Par.fork(mapReduce(l)(zero, map, reduce))
      val rf = Par.fork(mapReduce(r)(zero, map, reduce))
      Par.map2(lf, rf)(reduce(_, _))
    }

  def countWords(ps: IndexedSeq[String]): Par[Int] = {
    def map(paragraph: String): Int = paragraph split ("[\\s]") length
    def reduce(pCount1: Int, pCount2: Int) = pCount1 + pCount2

    mapReduce(ps)("", map, reduce)
  }

}
