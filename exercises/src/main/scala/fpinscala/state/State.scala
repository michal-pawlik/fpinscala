package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MinValue, rng2) => (Int.MaxValue, rng2)
    case (a, rng2) if a < 0 => (-a, rng2)
    case x => x
  }

  def double(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(_/(Int.MaxValue.toDouble+1))(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (j, rng2) = double(rng1)
    ((i,j), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, j), rng1) = intDouble(rng)
    ((j, i), rng1)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (i, rng1) = double(rng)
    val (j, rng2) = double(rng1)
    val (k, rng3) = double(rng2)
    ((i, j, k), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @annotation.tailrec
    def go(rng: RNG, c: Int, res: List[Int]): (List[Int], RNG) =
      if (c>0) {
        val (e, rng1) = rng.nextInt
        go(rng1, c-1, e::res)
      } else (res, rng)

    go(rng, count, Nil)
  }


  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    (rng: RNG) => {
      val (a, rng1) = ra(rng)
      map(rb)(b => f(a,b))(rng1)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def go(results: Rand[List[A]], input: List[Rand[A]]): Rand[List[A]] =
      input match {
        case h :: t => go(map2(h, results)((e, l) => e::l), t)
        case Nil => results
      }
    go(unit(Nil), fs)
  }

  def sequenceFold[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil:List[A])){ (e, acc) =>
      map2(e, acc)(_ :: _)
    }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    (rng: RNG) => {
      val (v1, rng1) = f(rng)
      g(v1)(rng1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) {e =>
      val mod = e % n
      if (e +(n-1) - mod > 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  def mapUsingFlatMap[A, B](f: Rand[A])(g: A =>B): Rand[B] =
    flatMap(f)(e => unit(g(e)))

  def map2UsingFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(x => State.unit(f(x)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(x => sb.map(y => f(x, y)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State{ s0: S =>
      val (a, s1) = run(s0)
      f(a).run(s1)
    }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Transition[S, +A] = S => (A, S)
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

  def unit[S, A](a: A) = State((s: S) => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(State.unit(Nil):State[S, List[A]])((f, acc) => f.map2(acc)(_ :: _))
}
