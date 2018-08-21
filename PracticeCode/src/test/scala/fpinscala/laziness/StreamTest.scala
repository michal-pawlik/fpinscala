package fpinscala.laziness

import org.scalatest.{Matchers, WordSpec}

class StreamTest extends WordSpec with Matchers {

  "use take and toList" in {
    List(1,2) shouldBe Stream(1,2,3).take(2).toListRecursive
  }

  "check for Infinite Stream" in {
    false shouldBe Stream.ones.forAll(_ != 1)
    true shouldBe Stream.ones.map(_ + 1).exists(_ != 1)
  }

  "scanRight should produce given List" in {
    List(6, 5, 3, 0) shouldBe Stream(1,2,3).scanRight(0)(_+_).toListRecursive
  }
}
