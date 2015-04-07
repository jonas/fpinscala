package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("empty");
      case Cons(head, tail) => tail
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("empty");
      case Cons(head, tail) => Cons(h, tail)
    }

  def dropChecked[A](l: List[A], n: Int): List[A] =
    (l, n) match {
      case (_, i) if i < 0 => sys.error("cannot drop negative number of elements");
      case (Nil, i) if i > 0 => sys.error("list is too small");
      case (Cons(_, tail), i) if i >= 1 => drop(tail, i - 1)
      case (_, 0) => l
    }

  def dropDoubleMatch[A](l: List[A], n: Int): List[A] =
    (l, n) match {
      case (_, i) if i <= 0 => l
      case (Nil, _) => Nil
      case (Cons(_, tail), _) => drop(tail, n - 1)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, tail) => if (n <= 0) l else drop(tail, n - 1)
    }

  def dropWhile1[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (!f(x)) l else dropWhile1(xs, f)
    }


  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => l
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  def length1[A](l: List[A]): Int =
    l match {
      case Nil => 0
      case Cons(x, xs) => 1 + length(xs)
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((a: A, x: Int) => x + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum311(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)
  def product311(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)
  def length311[A](k: List[A]): Int = foldLeft(k, 0)((acc, _) => acc + 1)
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((acc, x) => Cons(x, acc))

  def append[A](l: List[A], a: A): List[A] = foldRight(l, List(a))((x,acc) => Cons(x, acc))

  def concat[A](l: List[List[A]]): List[A] =
     foldRight(l, Nil:List[A])((l1, result) =>
       foldRight(l1, result)((x, acc) => Cons(x, acc)))

  def add1(l: List[Int]): List[Int] = 
     foldRight(l, Nil:List[Int])((x, acc) => Cons(x + 1, acc))

  def doubleToString(l: List[Double]): List[String] = 
     foldRight(l, Nil:List[String])((x, acc) => Cons(x.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
     foldRight(l, Nil:List[B])((x, acc) => Cons(f(x), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
     foldRight(as, Nil:List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
     foldRight(as, Nil:List[B])((x, acc) =>
       foldRight(f(x), acc)(Cons(_, _)))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
     flatMap(l)(a => if (f(a)) List(a) else Nil)


  def addLists(as: List[Int], bs: List[Int]): List[Int] =
     (as, bs) match {
       case (Nil, Nil) => Nil
       case (Cons(a, as1), Cons(b, bs1)) => Cons(a + b, addLists(as1, bs1))
       case _ => sys.error("m33p");
     }

  def zipWith[A, B](as: List[A], bs: List[A])(f: (A, A) => B): List[B] =
     (as, bs) match {
       case (Nil, Nil) => Nil
       case (Cons(a, as1), Cons(b, bs1)) => Cons(f(a, b), zipWith(as1, bs1)(f))
       case _ => sys.error("m33p");
     }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def go(sup: List[A], sub: List[A], strict: Boolean): Boolean = (sup, sub) match {
       case (_, Nil) => true
       case (Nil, _) => false
       case (Cons(a, as), Cons(x, xs)) =>
         if (a == x && go(as, xs, true))
	   true
	 else if (strict)
	   false
	 else
	   go(as, sub, false)
     }

    go(sup, sub, false)
  }
}
