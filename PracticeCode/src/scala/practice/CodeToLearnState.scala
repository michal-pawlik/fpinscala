package fpinscala

import fpinscala.state._

object CodeToLearnState extends App {

  val dummyRng = new RNG.Simple(42)

  println(dummyRng.nextInt)
  println(dummyRng.nextInt._2.nextInt)
  println("------")
  printRNGfewTimes(dummyRng, 3)

  def printRNGfewTimes(rng: RNG, times: Int): Unit = {
   if(times > 0) {
     val (value, nextState) = rng.nextInt
     println(value)
     printRNGfewTimes(nextState, times-1)
   }
  }


  def printRNGfewTimes2(rng: RNG, rand: RNG.Rand[Int], times: Int): Unit = {
    if(times > 0) {
      val (value, nextState) = rand(rng)
      println(value)
      printRNGfewTimes2(nextState,rand, times-1)
    }
  }
  println("MinValue " + Int.MinValue)
  println("MaxValue " + Int.MaxValue)
  println("MaxValue overflow " + (Int.MaxValue+1))

  println(RNG.double(dummyRng))
  println(RNG.double2(dummyRng))

  println(List.fill(1)(RNG.int)(0)(dummyRng))

  println(RNG.nonNegativeLessThan(1000)(dummyRng))

  printRNGfewTimes2(dummyRng, RNG.nonNegativeLessThan(2147483647), 10)

  println(Int.MaxValue % (Int.MaxValue + 1000))
}
