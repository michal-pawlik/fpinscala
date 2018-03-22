package scala.fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = {
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def product(ds: List[Double]): Double = {
    ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }
  }
//  foldLeft, which goes from head to tail, but foldRight has to start at the end of the list and work its way forward to the head.
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(h,t) => f(h, foldRight(t, z)(f))
    }
  }

  def sum2(ns: List[Int]) = {
    foldRight(ns, 0)(_ + _)
  }

  def product2(ns: List[Int]) = {
    foldRight(ns, 1)(_ * _)
  }

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(h,t) => t
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => sys.error("Empty List!")
      case Cons(_, t) => Cons(h,t)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    l match {
      case Nil => l
      case Cons(_,t) => drop(t, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h,t) => Cons(h, init(t))
    }
  }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @annotation.tailrec
    def go(ls:List[A]): List[A] = ls match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_,acc) => 1 + acc)
  }
//  In fact, it isn’t recursive at all. It is implemented as a while loop. On each iteration, the next list item is passed to the function f and the accumulator (called acc) is updated. When there are no more list items, the accumulator is returned. No recursion means no stack overflows.
  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z,h))(f)

    }

  def sum3(l: List[Int]) = {
    foldLeft(l, 0)(_ + _)
  }

  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))


  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B = {
    foldLeft(reverse(l), z)((acc,h) => f(h,acc))
  }

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = {
    foldRight(l, r)(Cons(_,_))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append)
  }

  def add1(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((h, t) => Cons(h+1, t))
  }

  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))
  }

}
