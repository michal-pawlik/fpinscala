package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {



  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  def size2[A](t: Tree[A]): Int = t match {
    case l: Leaf[A] => 1
    case b: Branch[A] => 1 + size2(b.left) + size2(b.right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case l: Leaf[A] => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int = {
    fold(t)(i => 1)(1 + _ + _)
  }

  def maximumViaFold(t: Tree[Int]): Int = {
    fold(t)(i => i)(_ max _)
  }

  def depthViaFold[A](t: Tree[A]): Int = {
    fold(t)(i => 0)(1 + _ max _)
  }

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(i => Leaf(f(i)): Tree[B])(Branch(_,_))
  }
}
