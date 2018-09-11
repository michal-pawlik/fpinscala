package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG)
}

// Till page 84
object RNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to
      // generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill.
      // The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }


  // We need to be quite careful not to skew the generator.
  // Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
  // it suffices to increment the negative numbers by 1 and make them positive.
  // This maps Int.MinValue to Int.MaxValue and -1 to 0.

  // juxtapose byte as Int example byte range is -128.. 127
  // so if it comes to -128 the -(-128) will return 128 which is invalid int, so increment the last -ve
  // -(-128 + 1) becomes 127 which is valid.

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  // We generate an integer >= 0 and divide it by one higher than the
  // maximum. This is just one possible solution.
  //
  // juxtapose byte as Int example byte range is -128.. 127
  // i will be between 0 and 127
  // i / (127+1) it will return double and range would be 0 and exclusive 1


  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    rng.nextInt match {
      case (i, r) if (count > 0) => (i :: ints(count - 1)(r)._1, r)
      case (i, r) => (List(), r)
    }
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0)
      (List(), rng)
    else {
      val (x, r1) = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)
    }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  val _double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  }

  /*
  https://forums.manning.com/posts/list/34423.page;jsessionid=8DD5BA1F0AF5BE655722722A77BE53D5
  Hi

I am confused about how the decision is made to recurse in nonNegativeLessThan().
 My understanding is that we have to exclude cases where our call to nonNegativeInt
 returns numbers that fall in the range greater than the largest multiple of n and less than Int.MaxValue.
 So for example if we say n is 1000. Then this means we have to exclude everything between 2147483000 and 2147483647.
 And when we get a number in this range we have to try again. However if that is the case the criteria (i + (n-1) - mod >= 0)
does not make sense to me. eg if i == 2147483073, this gives us a value of 73 for mod.
 Which implies (2147483073 + (1001) - 73) >= 0 and therefore we accept the number.
  I guess I am misunderstanding the premise. I would be grateful if somone could explain this to me

Many Thanks
Des

   */

  def nonNegativeLessThanWrong(n: Int): Rand[Int] =
    map(nonNegativeInt) {
      _ % n
    }


  def nonNegativeLessThan2(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan2(n)(rng2)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) {
      i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod)
        else nonNegativeLessThan(n)
    }
  }

  def mapUsingFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s) {
      a => unit(f(a))
    }
  }
}

import State._

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
    State(
      s => {
        val (a, s1) = run(s)
        (f(a), s1)
      }
    )
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    State(
      s => {
        val (a, s1) = run(s)
        val (b, s2) = sb.run(s1)
        (f(a, b), s2)
      }
    )
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(
      s => {
        val (a, s1) = run(s)
        f(a).run(s1)
      }
    )
  }

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // The idiomatic solution is expressed via foldRight
  def sequenceViaFoldRight[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
      actions match {
        case Nil => (acc.reverse, s)
        case h :: t => h.run(s) match {
          case (a, s2) => go(s2, t, a :: acc)
        }
      }

    State((s: S) => go(s, sas, List()))
  }

  def sequenceViaFoldLeft[S, A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update: Input => Machine => Machine = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    r <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)


  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    //      _ <- sequence(inputs.map(input => (modify[Machine] _).compose(update)(input)))
    _ <- sequence(inputs.map(input => modify[Machine](update(input))))
    s <- get
  } yield (s.coins, s.candies)
  // Here the modify will give you the State[Machine]((), Machine) and then the yield at the end will make use of map
  // and will change the State's A to the tuple of coins and candies, and will give the State[Machine],
  // State[Machine]((), Machine) Here yield will update the () to coins, candies,

}
