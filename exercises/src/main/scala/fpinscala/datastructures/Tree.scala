package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(l) => l
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(l) => Leaf(f(l))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(lf: A => B)(bf: (B, B) => B): B = t match {
    case Leaf(l)      => lf(l)
    case Branch(l, r) => bf(fold(l)(lf)(bf), fold(r)(lf)(bf))
  }

  def sizef[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _)

  def maximumf(t: Tree[Int]): Int = fold(t)(x => x)(_ max _)

  def depthf[A](t: Tree[A]): Int = fold(t)(_ => 0)((x, y) => 1 + (x max y))

  def mapf[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _))

}
