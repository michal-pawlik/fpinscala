package fpinscala.errorhandling

sealed trait Either[+E, +A] {

}

case class Left[+E](get :E) extends Either[E, Nothing]
case class Right[+A](get: A)extends Either[Nothing, A]

object Either {

}