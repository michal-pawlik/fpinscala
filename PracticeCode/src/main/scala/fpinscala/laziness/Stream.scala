package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def toListRecursive: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRecursive
    case _ => List()
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if(n > 1) => cons(h(), t().take(n-1))
    case Cons(h,_) if(n == 1) => cons(h() , empty)
    case _ => empty
  }

  final def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) if(n > 0) => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if f(h()) => cons(h(), t().takeWhile(f))
    case _ => empty
  }

  def headOption2:Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a,b) => p(a) || b)

  def forAll(f: A => Boolean): Boolean =
    foldRight(true)((a,b) => f(a) && b)

  def takeWhile_1(f: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a,b) => if (f(a)) cons(a, b) else empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty:Stream[B])((a,b) => cons(f(a),b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty:Stream[A])((a,b) => if (f(a)) cons(a,b) else b)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a,b) => cons(a,b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((a,b) => f(a).append(b))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some(f(h()), t())
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this,n)) {
      case (Cons(h, _), 1) => Some((h(), (empty[A],0)))
      case (Cons(h, t), n) if(n > 1) => Some((h(), (t(),n-1)))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h,t) if (f(h())) => Some(h(), t())
      case _ => None
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some(f(h1(),h2()), (t1(), t2()))
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty).forAll({
      case (o1,o2) => o1 == o2
    })


  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)


  def tails2: Stream[Stream[A]] =
    unfold(this){
      case Empty => None
      case Cons(h,t) => Some((Cons(h,t), t()))
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h,t) => if(f(h())) Some(h()) else t().find(f)
  }
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

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def constant2[A](a: A): Stream[A] = {
    cons(a, constant2(a))
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0+f1))
    go(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }
  }

  def unfoldViaFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).fold(empty[A])((p:(A,S)) => cons(p._1, unfold(p._2)(f)))

  def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map((p:(A,S)) => cons(p._1, unfold(p._2)(f))).getOrElse(empty[A])

  val fibsViaUnfold =
    unfold((0,1)) (p => p match {case (f0,f1) => Some((f0, (f1, f0+f1)))})

  def fromViaUnfold(n: Int) =
    unfold(n)(x => Some(x, x+1))

  def constantViaUnfold[A](a: A) =
    unfold(a)(_ => Some(a,a))

  val onesViaUnfold = unfold(1)(_ => Some(1,1))
}