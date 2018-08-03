package fpinscala.errorhandling

//pg 54
sealed trait Option[+A] {

}
object Option {

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

