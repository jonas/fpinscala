package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = a1 compose a2
    val zero = (a: A) => a
  }

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], g: Gen[A]): Prop = {
    val triple: Gen[(A, A, A)] =
      for { a <- g; b <- g; c <- g } yield (a, b, c)

    val opsAssociativity = Prop.forAll(triple) {
      case (a, b, c) => m.op(m.op(a, b), c) == m.op(a, m.op(b, c))
    }

    val zeroIdentity = Prop.forAll(g) {
      a => m.op(m.zero, a) == m.op(a, m.zero)
    }

    opsAssociativity && zeroIdentity
  }


  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero) { (b, a) => m.op(b, f(a)) }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid: Monoid[B => B])(f curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid: Monoid[B => B])(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.size <= 1)
      as.headOption map(f) getOrElse m.zero
    else { 
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, endoMonoid: Monoid[Int => Int])(
      a => b => if (a <= b) a else Int.MinValue
    )(ints.lastOption.getOrElse(Int.MaxValue)) > Int.MinValue

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = 
    new Monoid[Par[A]] {
      def op(a1: Par[A], a2: Par[A]) = (a1 map2 a2)(m.op(_, _))
      val zero = Par.unit(m.zero)
    }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    if (v.length <= 1)
      Par.asyncF((a: Option[A]) => a map(f) getOrElse(m.zero))(v.headOption)
    else {
      val (l, r) = v.splitAt(v.length / 2)
      val lf = Par.fork(parFoldMap(l, m)(f))
      val rf = Par.fork(parFoldMap(r, m)(f))
      Par.map2(lf, rf)(m.op(_, _))
    }

  def wordsFor(s: String*) =
    s.mkString.length min 1

  def wcMonoid: Monoid[WC] =
    new Monoid[WC] {
      def op(a1: WC, a2: WC) = (a1, a2) match {
        case (Stub(c1),         Stub(c2))         => Stub(c1 + c2)
        case (Stub(c1),         Part(l2, w2, r2)) => Part(c1 + l2, w2, r2)
        case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + wordsFor(r1, l2) + w2, r2)
        case (Part(l1, w1, r1), Stub(c2))         => Part(l1, w1, r1 + c2)
      }
      val zero = Stub("")
    }

  def count(s: String): Int = {
    def split(s: String): Vector[String] = {
      if (s.length <= 10)
        Vector(s)
      else {
        val l = s.substring(0, s.length / 2)
        val r = s.substring(s.length / 2)
        split(l) ++ split(r)
      }
    }

    foldMapV(split(s), wcMonoid)((s: String) => {
      val words = s.split("\\s+", -1)

      if (words.length <= 1)
        Stub(s)
      else if (words.length <= 2)
        Part(words(0), 0, words(1))
      else
        Part(words(0), words.length - 2, words(words.length - 1))
    }) match {
      case Stub(c) => wordsFor(c)
      case Part(l, w, r) => wordsFor(l) + w + wordsFor(r)
    }
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(l: (A, B), r: (A, B)) = (A.op(l._1, r._1), B.op(l._2, r._2))
      val zero = (A.zero, B.zero)
    }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    sys.error("todo")

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero) { (b, a) => mb.op(b, f(a)) }

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(Nil:List[A])(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(l)      => f(l)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    as match {
      case Leaf(l)      => f(z, l)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    as match {
      case Leaf(l)      => f(l, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Some(v) => mb.op(f(v), mb.zero)
      case None => mb.zero
    }
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    as match {
      case Some(v) => f(z, v)
      case None => z
    }
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    as match {
      case Some(v) => f(v, z)
      case None => z
    }
}

