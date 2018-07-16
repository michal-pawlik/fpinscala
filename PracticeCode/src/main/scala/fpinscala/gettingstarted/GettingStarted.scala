package scala.fpinscala.gettingstarted

object Module_1 extends App {

  //  def fib(n: Int): Int = {
  //    if (n == 0 || n == 1) {
  //      n
  //    } else {
  //      fib(n-1) + fib(n-2)
  //    }
  //  }
  def fib(n: Int): Int = {
    def loop(n: Int, prev: Int, cur: Int): Int = {
      if (n <= 1) {
        prev
      } else {
        loop(n - 1, cur, prev + cur)
      }
    }

    loop(n, 0, 1)
  }


  // 0 1 1 2 3 5 8 13 21

  //  println(fib(5))
  //  println(fib(6))
  //  println(fib(7))

  assert(fib(6) == 5)
}

object Module_2 extends App {

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    def loop(index: Int): Boolean = {
      if (index >= as.size - 1) {
        true
      } else if (gt(as(index + 1), as(index))) {
        loop(index + 1)
      } else {
        false
      }
    }

    loop(0)
  }


  println(isSorted[Int](Array(1, 2, 3, 4), (x, y) => x > y))

  //  assert(isSorted[Int](Array(1,2,3,4), (x,y) => x > y ), true)
}

object Modele_3 extends App {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    //    (a:A) => {b:B => f(a,b)}
    a => b => f(a, b)
  }


  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {

  //    a => f(g(a))
  a => g.andThen(f)(a)
  }

  val f: String => Int = s => s.length
  val g: Int => Int = i => i * 2
  val h = f.andThen(g) // x => g(f(x))
//  val h1 = g.andThen(f)

  println(h("dhiraj"))
}
