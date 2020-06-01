package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size(n: Tree[A]): Int = n match {
    case Nil => 0
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r);
  }

  def maximum(t: Tree[A]): Int = t match {
    case Leaf(l) => l
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth(t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => depth(l) max depth(r)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A], z: A)(f: A => B)(g: (B,B) => B) = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]) : Int =
    fold(t)(a => 1)(1+_+_)
}
