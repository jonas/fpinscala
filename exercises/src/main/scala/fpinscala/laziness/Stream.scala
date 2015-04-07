package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  final def toList: List[A] = this match {
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty      => empty
    case Cons(h, t) =>
      if (n <= 0) empty
      else cons(h(), t().take(n - 1))
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Empty      => empty
    case Cons(h, t) =>
      if (n <= 0) this
      else t().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def headOption: Option[A] =
    foldRight(None:Option[A])((a, b) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](b: => Stream[B]): Stream[B] =
    foldRight(b)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight(empty[B])((a, b) => f(a) append b)

  // def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  //   f(z) map(o => Stream.cons(o._1, unfold(o._2)(f))) getOrElse(Stream.empty)

  def mapWithUnfold[B](f: A => B): Stream[B] =
    unfold(this)(x => x match {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    })

  def takeWithUnfold(n: Int): Stream[A] =
    unfold((this, n))(x => x match {
      case (Cons(h, t), n) if n > 0 => Some((h()), (t(), n - 1))
      case _ => None
    })

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] =
    unfold(this)(x => x match {
      case Cons(h, t) if (p(h())) => Some((h()), t())
      case _ => None
    })

  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, that))(x => x match {
      case (Cons(ah, at), Cons(bh, bt)) => Some(f(ah(), bh()), (at(), bt()))
      case _ => None
    })

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s2))(x => x match {
      case (Cons(ah, at), Cons(bh, bt)) => Some((Some(ah()), Some(bh())), (at(), bt()))
      case (Cons(ah, at), Empty) => Some((Some(ah()), None), (at(), x._2))
      case (Empty, Cons(bh, bt)) => Some((None, Some(bh())), (x._1, bt()))
      case _ => None
    })

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith1[B](s: Stream[B]): Boolean =
    zipWith(s)((a, b) => a == b) forAll(x => x)

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s) takeWhile(!_._2.isEmpty) forAll { case (a, b) => a == b }

  def tails: Stream[Stream[A]] =
    unfold(this)(x => x match {
      case Cons(h, t) => Some((x, t()))
      case _ => None
    }) append (Stream(empty))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] =
      Stream.cons(a, go(b, a + b))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) map(o => Stream.cons(o._1, unfold(o._2)(f))) getOrElse(Stream.empty)

  def ones1: Stream[Int] = unfold(1)(x => Some(x, x))

  def constant1[A](a: A): Stream[A] = unfold(a)(x => Some(a, a))

  def from1(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))

  def fibs1: Stream[Int] =
    unfold((0, 1))(x => Some(x._1, (x._2, x._1 + x._2)))
}
