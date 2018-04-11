package fpinscala

import fpinscala.state._

object CodeToLearnState extends App {

  println(new RNG.Simple(42).nextInt)
  println(new RNG.Simple(42).nextInt._2.nextInt)
  println("------")
  printRNGfewTimes(new RNG.Simple(42), 3)

  def printRNGfewTimes(rng: RNG, times: Int): Unit = {
   if(times > 0) {
     val (value, nextState) = rng.nextInt
     println(value)
     printRNGfewTimes(nextState, times-1)
   }
  }
  println("MinValue " + Int.MinValue)
  println("MaxValue " + Int.MaxValue)
  println("MaxValue overflow " + (Int.MaxValue+1))

  println(RNG.double(new RNG.Simple(42)))
  println(RNG.double2(new RNG.Simple(42)))
}
