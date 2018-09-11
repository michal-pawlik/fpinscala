package fpinscala.state

import fpinscala.state.RNG.Simple
import org.scalatest.{FunSuite, Matchers, WordSpec}

class StateTest extends WordSpec with Matchers {

  "print Hello" in {
    "Hello" shouldBe "Hello"
  }

  "should return random List" in {
    val rand = Simple(10)
    List(3847489, 1334288366, 1486862010) shouldBe RNG.ints(3)(rand)._1
  }

  "try non negative number mod" in {
    val rand = Simple(10)
    println(RNG.nonNegativeLessThan(32)(rand))
  }

}
