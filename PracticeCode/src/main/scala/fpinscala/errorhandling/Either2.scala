//package fpinscala.errorhandling
//
//sealed trait Either[+E, +A] {
//
//  def map[B](f: A => B): Either[E, B] =
//    this match {
//      case Right(a) => Right(f(a))
//      case Left(e) => Left(e)
//  }
//
//  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
//    this match {
//      case Left(e) => Left(e)
//      case Right(a) => f(a)
//    }
//
//  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] =
//    this match {
//      case Left(_) => b
//      case Right(a) => Right(a)
//    }
//
//  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C) :  Either[EE, C] =
//    for {
//      a <- this
//      bb <- b
//    } yield f(a,bb)
//
//  def map2_2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C) :  Either[EE, C] =
//    this flatMap(aa => b map(bb => f(aa,bb)))
//
//  def map2_1[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C) :  Either[EE, C] =
//    this match {
//      case Left(e) => Left(e)
//      case Right(a) => b match {
//        case Left(e) => Left(e)
//        case Right(bb) => Right(f(a,bb))
//      }
//    }
//}
//
//case class Left[+E](get :E) extends Either[E, Nothing]
//case class Right[+A](get: A)extends Either[Nothing, A]
//
//object Either {
//
//  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
//    es match {
//      case Nil => Right(Nil)
//      case h :: t => (f(h) map2(traverse(t)(f)))(_ :: _)
//    }
//
//}