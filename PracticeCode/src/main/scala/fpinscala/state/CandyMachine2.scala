package fpinscala.state

import fpinscala.state.CandyMachine2.Candy.update

object CandyMachine2 extends App {

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

  /*
    Candy dispenser. The machine has two types of input: you can
    insert a coin, or you can turn the knob to dispense candy. It can be in one of two
    states: locked or unlocked. It also tracks how many candies are left and how many
    coins it contains.

    sealed trait Input
    case object Coin extends Input
    case object Turn extends Input
    case class Machine(locked: Boolean, candies: Int, coins: Int)

     The rules of the machine are as follows:
    •	Inserting a coin into a locked machine will cause it to unlock if there’s any
      candy left.
    •	Turning the knob on an unlocked machine will cause it to dispense candy and
      become locked.
    •	Turning the knob on a locked machine or inserting a coin into an unlocked
      machine does nothing.
    •	A machine that’s out of candy ignores all inputs.


   */

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object Candy {
    // Here input is given and Machine => Machine is returned
    def update =
      (i: Input) =>
        (s: Machine) =>
          (i, s) match {
            case (_, Machine(_, 0, _))        => s
            case (Coin, Machine(false, _, _)) => s
            case (Turn, Machine(true, _, _))  => s
            case (Coin, Machine(true, candy, coin)) =>
              Machine(false, candy, coin + 1)
            case (Turn, Machine(false, candy, coin)) =>
              Machine(true, candy - 1, coin)
      }

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
      for {
        _ <- sequence(inputs map (modify[Machine] _ compose update))
        s <- get
      } yield (s.coins, s.candies)

    def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] =
      for {
        //      _ <- sequence(inputs.map(input => (modify[Machine] _).compose(update)(input)))
        _ <- sequence(inputs.map(input => modify[Machine](update(input))))
        s <- get
      } yield (s.coins, s.candies)

    // Here the modify will give you the State[Machine]((), Machine) and then the yield at the end will make use of map
    // and will change the State's A to the tuple of coins and candies, and will give the State[Machine],
    // State[Machine]((), Machine) Here yield will update the () to coins, candies,

  }

  //candies: Int, coins: Int
  println(Candy.simulateMachine2(Coin :: Turn :: Nil).run((Machine(true, 2, 8))))
  //  ((9,1),Machine(true,1,9))

  val var1 = sequence((Coin :: Turn :: Nil).map(input => modify[Machine](update(input))))
    .run((Machine(true, 2, 8)))
  println(var1)

  println(Candy.update(Coin)(Machine(true, 2, 8)))

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

}
