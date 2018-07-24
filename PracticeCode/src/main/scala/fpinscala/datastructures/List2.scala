//package fpinscala.datastructures
//
//import scala.annotation.tailrec
//
//sealed trait List[+A]
//
//case object Nil extends List[Nothing]
//
//case class Cons[+A](head: A, tail: List[A]) extends List[A]
//
//object List {
//
//  def sum(ints: List[Int]): Int = {
//    ints match {
//      case Nil => 0
//      case Cons(x, xs) => x + sum(xs)
//    }
//  }
//
//  def product(ds: List[Double]): Double = {
//    ds match {
//      case Nil => 1.0
//      case Cons(0.0, _) => 0.0
//      case Cons(x, xs) => x * product(xs)
//    }
//  }
//
//  def apply[A](as: A*): List[A] = {
//    if (as.isEmpty) Nil
//    else Cons(as.head, apply(as.tail: _*))
//  }
//
//  def append[A](a1: List[A], a2: List[A]): List[A] = {
//    a1 match {
//      case Nil => a2
//      case Cons(h,t) => Cons(h, append(t, a2))
//    }
//  }
////  foldLeft, which goes from head to tail, but foldRight has to start at the end of the list and work its way forward to the head.
//  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
//    as match {
//      case Nil => z
//      case Cons(h,t) => f(h, foldRight(t, z)(f))
//    }
//  }
//
//  def sum2(ns: List[Int]) = {
//    foldRight(ns, 0)(_ + _)
//  }
//
//  def product2(ns: List[Int]) = {
//    foldRight(ns, 1)(_ * _)
//  }
//
//  def tail[A](l: List[A]): List[A] = {
//    l match {
//      case Nil => sys.error("tail of empty list")
//      case Cons(h,t) => t
//    }
//  }
//
//  def setHead[A](l: List[A], h: A): List[A] = {
//    l match {
//      case Nil => sys.error("Empty List!")
//      case Cons(_, t) => Cons(h,t)
//    }
//  }
//
//  def drop[A](l: List[A], n: Int): List[A] = {
//    if (n <= 0) l
//    l match {
//      case Nil => l
//      case Cons(_,t) => drop(t, n-1)
//    }
//  }
//
//  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
//    l match {
//      case Cons(h,t) if f(h) => dropWhile(t, f)
//      case _ => l
//    }
//  }
//
//  def init[A](l: List[A]): List[A] = {
//    l match {
//      case Nil => sys.error("init of empty list")
//      case Cons(_, Nil) => Nil
//      case Cons(h,t) => Cons(h, init(t))
//    }
//  }
//
//  def init2[A](l: List[A]): List[A] = {
//    import collection.mutable.ListBuffer
//    val buf = new ListBuffer[A]
//
//    @annotation.tailrec
//    def go(ls:List[A]): List[A] = ls match {
//      case Nil => sys.error("init of empty list")
//      case Cons(_,Nil) => List(buf.toList: _*)
//      case Cons(h,t) => buf += h; go(t)
//    }
//    go(l)
//  }
//
//  def length[A](l: List[A]): Int = {
//    foldRight(l, 0)((_,acc) => 1 + acc)
//  }
////  In fact, it isn’t recursive at all. It is implemented as a while loop. On each iteration, the next list item is passed to the function f and the accumulator (called acc) is updated. When there are no more list items, the accumulator is returned. No recursion means no stack overflows.
//  @tailrec
//  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
//    l match {
//      case Nil => z
//      case Cons(h,t) => foldLeft(t, f(z,h))(f)
//
//    }
//
//  def sum3(l: List[Int]) = {
//    foldLeft(l, 0)(_ + _)
//  }
//
//  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)
//
//  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)
//
//  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))
//
//
//  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B = {
//    foldLeft(reverse(l), z)((acc,h) => f(h,acc))
//  }
//
//  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = {
//    foldRight(l, r)(Cons(_,_))
//  }
//
//  def concat[A](l: List[List[A]]): List[A] = {
//    foldRight(l, Nil: List[A])(append)
//  }
//
//  def add1(l: List[Int]): List[Int] = {
//    foldRight(l, Nil: List[Int])((h, t) => Cons(h+1, t))
//  }
//
//  def doubleToString(l: List[Double]): List[String] = {
//    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))
//  }
//
//  def map[A,B](l: List[A])(f: A => B): List[B] = {
//    foldRight(l, Nil: List[B])((h,acc) => Cons(f(h), acc))
//  }
//
//  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
//    foldRight(l, Nil: List[A])((h,acc) => if(f(h)) Cons(h, acc) else acc)
//  }
//
//  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
//    concat(map(l)(f))
//  }
//
//  def flatMap2[A,B](l: List[A])(f: A => List[B]): List[B] = {
//    foldRight(l, Nil: List[B])((h,acc) => append(f(h), acc))
//  }
//
//  def flatMap3[A,B](l: List[A])(f: A => List[B]): List[B] = {
//    concat(foldRight(l, Nil: List[List[B]])((h,t) => Cons(f(h), t)))
//  }
//
//  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
//    flatMap(l)(a => if (f(a)) List(a) else Nil)
//  }
//
//  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
//    case (Nil, _) => Nil
//    case (_, Nil) => Nil
//    case (Cons(h1, t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
//
//  }
//
//  def addPairwise2(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
//    case (Nil, Nil) => Nil
//    case (Cons(h1, t1), Nil) => Cons(h1, t1)
//    case (Nil, Cons(h2,t2)) => Cons(h2,t2)
//    case (Cons(h1, t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise2(t1,t2))
//  }
//
//  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
//    case (Nil, _) => Nil
//    case (_, Nil) => Nil
//    case (Cons(h1, t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
//  }
//
//
//  @annotation.tailrec
//  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
//    case (_,Nil) => true
//    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
//    case _ => false
//  }
//
//  @annotation.tailrec
//  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
//    case Nil => sub == Nil
//    case _ if startsWith(sup, sub) => true
//    case Cons(h,t) => hasSubsequence(t, sub)
//  }
//
//  @annotation.tailrec
//  def hasSubsequence2[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
//    case (_, Nil) => true
//    case (Cons(h1,t1), Cons(h2,t2)) => if (h1 == h2) hasSubsequence2(t1,t2) else hasSubsequence2(t1,prefix)
//    case (Nil, p) => Nil == p
//  }
//
//
//}
