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


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case _ => Nil // could throw as well
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def go(sl: List[A], idx: Int): List[A] = sl match {
      case Cons(_, tail) if(idx==0) => tail
      case Cons(_, tail) if(idx>0) => go(tail, idx-1)
      case Nil => Nil
    }
    go(l, n)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(e, tail) if f(e) => dropWhile(tail, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = {

    def go(result: List[A], input: List[A]): List[A] = input match {
      case Cons(e, Nil) => append(result, List(e))
      case Cons(e, tail) => go(append(result, List(e)), tail)
      case _ => input
    }

    go(Nil, l)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((e, acc) => acc+1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(e, tail) => foldLeft(tail, f(z, e))(f)
  }
  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc,h) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((acc, e) => Cons(e, acc))

  def appendFold[A](l1: List[A], l2: List[A]): List[A] = foldRight(l2, l1)(Cons(_,_))

  def flatten[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])((e, acc) => foldRight(e, acc)(Cons(_,_)))

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((e, acc) => Cons(e+1, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((e, acc) => Cons(f(e), acc))

  def filter[A](l: List[A])(f: A=> Boolean): List[A] =
    foldRight(l, Nil:List[A])((e, acc) => if(f(e)) Cons(e, acc) else acc)

  def flatMap[A](l: List[A])(f: A=>List[A]): List[A] =
    flatten(map(l)(f))

  def filter2[A](l: List[A])(f: A=>Boolean): List[A] =
    flatMap(l)(e => if(f(e)) Cons(e, Nil) else Nil)

  def zip[A, B](l1: List[A], l2: List[B]): List[(A, B)] = {
    def go(result: List[(A, B)], in1: List[A], in2: List[B]): List[(A, B)] = (in1, in2) match {
      case (Nil, _) => result
      case (_, Nil) => result
      case (Cons(e1, t1), Cons(e2, t2)) => go(append(result, List((e1, e2))), t1, t2)
    }
    go(Nil, l1, l2)
  }

  def sumLists(l1: List[Int], l2: List[Int]): List[Int] = map(zip(l1, l2))(x => x._1 + x._2)

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: ((A, B)) => C): List[C] =
    map(zip(l1, l2))(f)
}
