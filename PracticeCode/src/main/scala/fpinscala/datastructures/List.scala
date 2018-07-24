package scala.fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]
// Start Pg.41
object List {
  def printHello: String = "Hello List"

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Cons(h, t) => t
      case Nil => sys.error("asdg")
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_,t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_,t) if (n>0) => drop(t, n-1)
    case _ => l
  }

  def dropWhile[A](l: List[A])( f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h,t) if f(h) => dropWhile(t)( f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, Nil) => Nil
    case Cons(h,t) => Cons(h, init(t))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def product(ns: List[Double])(f:(Double,Double)=>Double): Double =
    ns match {
      case Nil => 1.0
      case Cons(h,t) if(h == 0) => 0
      case Cons(h,t) => f(h, product(t)(f))
    }

  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(_,t) => 1 + length(t)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def sum3(l: List[Int]):Int = foldLeft(l, 0)(_+_)
  def product3(l: List[Double]):Double = foldLeft(l, 1.0)(_*_)
  def length3[A](l: List[A]): Int = foldLeft(l, 0)((x,y) => 1+x)
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((acc,x) => Cons(x,acc))

}
