package scala.fpinscala.gettingstarted

// On Page 21

object GettingStarted {

  def printHello = "Hello"

  def fibonacci(n: Int): Int = {
    n match {
      case _ if n < 1 => 0
      case 1 => 0
      case 2 => 1
      case _ => fibonacci(n-1) + fibonacci(n-2)
    }
  }

  def isSorted[A](arr: Array[A], ordered: (A,A) => Boolean): Boolean = {
    var sorted = true
    for(i <- 0 until arr.size - 1) {
      if (!(sorted && ordered(arr(i), arr(i+1)))) {
        sorted = false
      }
    }
    sorted
  }

  def isSorted2[A](arr: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def go(n:Int): Boolean = {
      n match {
        case _ if n >= arr.size - 1 => true
        case _ if !ordered(arr(n), arr(n+1)) => false
        case _ => go(n+1)
      }
    }
    go(0)
  }

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a:A => b:B => f(a,b)
  }

  def unCurry[A,B,C](f: A => B => C): (A,B) => C = {
    (a:A,b:B) => f(a)(b)
  }

  def compose[A,B,C](doubleMe: A => B, squareMe: B => C): A => C = {
    a:A => squareMe(doubleMe(a))
  }


}