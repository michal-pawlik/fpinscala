package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(v) => Right(f(v))
   case l @Left(_) => l
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case l@ Left(_) => l
   case Right(v) => f(v)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Left(_) => b
   case _ => this
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
   case (Right(r1), Right(r2)) => Right(f(r1, r2))
   case (l @Left(_), _) => l
   case (_, l @Left(_)) => l
 }

  def map2Better[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for{
      e1 <- this
      e2 <- b
    } yield f(e1,e2)
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {

  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case head :: tail => f(head).map2(traverse(tail)(f))(_ :: _)
    case Nil => Right(Nil)
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = traverse(es){x => x}

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}