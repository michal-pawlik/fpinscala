package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n>1 => cons(h(), t().take(n-1))
    case Cons(h, _) if n==0 => cons(h(), empty)
    case _ => this
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((e, acc) => p(e) && acc)

  def takeWhileFolding(p: A => Boolean): Stream[A] =
    this.foldRight(Empty: Stream[A])((e, acc) => if(p(e)) cons(e, acc) else empty)

  def headOption: Option[A] = this.foldRight(None: Option[A]){
    (e, acc) =>
    if(acc.isEmpty) Some(e)
    else None
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A=>B): Stream[B] = foldRight(Empty: Stream[B]){ (e, acc) =>
    cons(f(e), acc)
  }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((e, acc) => if(p(e)) cons(e, acc) else acc)

  def append[B>:A](s: =>Stream[B]): Stream[B] =
    foldRight(s)((e,acc) => cons(e, acc))

  def flatMap[B](f: A=>Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((e, acc) => f(e) append acc )

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).forAll {
      case (Some(v1), Some(v2)) => v1 == v2
      case (Some(_), None) => true
      case _ => false
    }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }

  def mapUnFold[B](f: A=>B): Stream[B] =unfold(this){
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }

  def takeUnFold(n: Int): Stream[A] = unfold((this, n)){
    case (Cons(h, _), 1) => Some(h(), (Empty, n))
    case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n-1))
    case _ => None
  }

  def takeWhileUnFold(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B, C](s2: Stream[B])(f: ((A, B)) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def zipAll[B, C](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
    case _ => None
  }

  def tails: Stream[Stream[A]] = unfold(this){
    case s @ Cons(_, t) => Some((s, t()))
    case Empty => None
  } append Stream(empty)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def go(curr: Int, prev: Int): Stream[Int] = {
      cons(curr, go(curr+prev, curr))
    }
    go(1, 0)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  def fibs_1: Stream[Int] = unfold((0,1))(s => Some((s._1, (s._2, s._1+s._2))))

}