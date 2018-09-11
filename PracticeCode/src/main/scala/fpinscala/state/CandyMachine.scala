package fpinscala.state

object CandyMachine extends App {

  import State._

  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map3[B](f: A => B): State[S, B] =
      State(s => {
        val (a, s1) = run(s)
        (f(a), s1)
      })

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(s => {
        val (a, s1) = run(s)
        f(a).run(s1)
      })
  }

  object State {

    def unit[S, A](a: A): State[S, A] =
      State(s => (a, s))

    // The idiomatic solution is expressed via foldRight
    def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
      sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

    def modify[S](f: S => S): State[S, Unit] =
      for {
        s <- get       // Gets the current state and assigns it to `s`.
        _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
      } yield ()

    def modify2[S](f: S => S): State[S, Unit] =
      get.flatMap(s => set(f(s)))

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }

  case class Acc(seed: Int)

  object Acc {
    def updateAcc =
      (s: Int) =>
        (acc: Acc) => {
          Acc(acc.seed + s)
      }

    def add(inputs: List[Int]): State[Acc, Int] = {
      for {
        _ <- sequence(inputs.map(input => modify[Acc](updateAcc(input))))
        s <- get
      } yield (s.seed)
    }

  }
  println(Acc.add(1 :: 2 :: 3 :: 4 :: Nil).run(Acc(1)))

  println(Acc.updateAcc(1))         // 1 => Acc => Acc
  println(Acc.updateAcc(1)(Acc(1))) // (1)(Acc(1))
  println(modify(Acc.updateAcc(1)))
  println(modify(Acc.updateAcc(1)).run(Acc(1)))

//  object Temp {
//    def modify(f: Int => Int): Int =
//      for {
//        s <- get1    // Gets the current state and assigns it to `s`.
//        _ <- set1(f(s)) // Sets the new state to `f` applied to `s`.
//      } yield ()
//
//    def get1 = 1
//
//    def set1(s: Int) = s
//  }
//  println(Temp.modify(_ + 1))

}
