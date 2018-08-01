package fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]
// Start Pg.44
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
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) if n > 0 => drop(t, n - 1)
    case _ => l
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def product(ns: List[Double])(f: (Double, Double) => Double): Double =
    ns match {
      case Nil => 1.0
      case Cons(h, t) if h == 0 => 0
      case Cons(h, t) => f(h, product(t)(f))
    }

  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(_, t) => 1 + length(t)
  }

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def length3[A](l: List[A]): Int = foldLeft(l, 0)((x, y) => 1 + x)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, x) => Cons(x, acc))

  def map1[A, B](l: List[A])(f: A => B): List[B] = {
    l match {
      case Cons(h, t) => Cons(f(h), map1(t)(f))
      case Nil => Nil
    }
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))
  }

  def filter1[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Cons(h, t) if f(h) => Cons(h, filter1(t)(f))
      case Cons(h, t) => filter1(t)(f)
      case Nil => Nil
    }
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    concat(map(l)(f))
  }

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(h=> if(f(h)) List(h) else Nil)

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case Cons(h,t) => if (startsWith(sup, sub)) true else hasSubsequence(t, sub)
  }

  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_, Nil) => true
    case (Cons(h1,t1), Cons(h2,t2)) => if (h1 == h2) startsWith(t1,t2) else false
    case _ => false
  }

}
