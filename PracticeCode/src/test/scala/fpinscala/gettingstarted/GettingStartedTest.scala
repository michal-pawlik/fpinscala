package fpinscala.gettingstarted

import org.scalatest.WordSpec

import scala.fpinscala.gettingstarted.GettingStarted

class GettingStartedTest extends WordSpec {

  ".printHello" when {
    "will return Hello" in {
      assertResult("Hello") {
        GettingStarted.printHello
      }
    }
  }

  ".fibonacci" when {
    "will return fibonacci by position" in {
      "1st fibanacci is 0"
      assertResult(0) {
        GettingStarted.fibonacci(1)
      }
      "2nd fibanacci is 1"
      assertResult(1) {
        GettingStarted.fibonacci(2)
      }
      "3rd fibanacci is 1"
      assertResult(1) {
        GettingStarted.fibonacci(3)
      }
      "4th fibanacci is 2"
      assertResult(2) {
        GettingStarted.fibonacci(4)
      }
      "5th fibanacci is 3"
      assertResult(3) {
        GettingStarted.fibonacci(5)
      }
      "6th fibanacci is 5"
      assertResult(5) {
        GettingStarted.fibonacci(6)
      }
    }
  }

  ".isSorted" when {
    "will return accordingly if its Sorted List" in {
      assertResult(true) {
        GettingStarted.isSorted[Int](Array(1,2,3,4), (x,y) => x < y)
      }
      assertResult(false) {
        GettingStarted.isSorted[Int](Array(1,2,4,3), (x,y) => x < y)
      }
      assertResult(true) {
        GettingStarted.isSorted[String](Array("a","b","c","d"), (x,y) => x.compareTo(y) < 0)
      }
      assertResult(false) {
        GettingStarted.isSorted[String](Array("b","a","c","d"), (x,y) => x.compareTo(y) < 0)
      }
    }
  }

  def div(a:Int, b:Int):Int = {
    a/b
  }


  def div2(a:Int): Int => Int = {
    b:Int => a/b
  }

  "curry and uncurry" should {
    "return the same result" in {
      assertResult(5) {
        GettingStarted.curry(div)(11)(2)
      }
      assertResult(5) {
        GettingStarted.unCurry(div2)(11,2)
      }
    }
  }

  def doubleMe(a:Int): Int= {
    a * 2
  }

  def squareMe(a:Int):Int = {
    Math.pow(a,2).toInt
  }

  ".compose" should {
    "Compose two function into One" in {
      assertResult(100) {
        GettingStarted.compose(doubleMe, squareMe)(5)
      }
    }
  }

}
