package fpinscala.errorhandling


import scala.annotation.tailrec
import scala.{Either => _, Option => _, Some => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    Option.flatten(this.map(f))

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case x @ Some(_) => x
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(v) if f(v) => Some(v)
    case _ => None
  }

  def nonEmpty: Boolean = this match {
    case None => false
    case _ => true
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def flatten[A](x: Option[Option[A]]): Option[A] = x match {
    case Some(x: Option[A]) => x
    case None => None
  }

  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(x), Some(y)) => Some(f(x,y))
    case _ => None
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    if(a.forall(_.nonEmpty)) Some(a.map { case Some(x) => x })
    else None

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @tailrec
    def go(processed: List[B], rest: List[A]): Option[List[B]] = rest match {
      case e :: tail =>
        val x = f(e)
        if(x.nonEmpty) go((x match {case Some(v) => v}) :: processed, tail)
        else None
      case Nil => Some(processed)
    }

    go(Nil, a)
  }

  def traverseMap2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case head :: tail => map2(f(head), traverse(tail)(f))((h, t) => h::t)
  }


}