package fpinscala.errorhandling

//62
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => f(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(v) => b
    case Right(v) => Right(v)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap(aa => b map (bb => f(aa,bb)))


  def map2_1[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa,bb)

}

case class Left[+E](get :E) extends Either[E, Nothing]
case class Right[+A](get: A)extends Either[Nothing, A]

object Either {

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    es.foldRight[Either[E,List[A]]](Right(Nil))((x,y) => x.map2(y)(_::_))

  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(Nil))((x,y) => f(x).map2(y)(_::_))



}