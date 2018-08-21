package fpinscala.datastructures

import org.scalatest.{Matchers, WordSpec}

class ListTest extends WordSpec  with Matchers {

  ".printHello" when {
    "will return Hello" in {
      assertResult("Hello List") {
        List.printHello
      }
    }
  }

  "should drop n items from list" in {
    assertResult(List(3,4,5)) {
      List.drop(List(1,2,3,4,5), 2)
    }
  }

  "should drop items from list until condition " in {
    assertResult(List(3,4,5)) {
      List.dropWhile(List(1,2,3,4,5))( _ < 3)
    }
  }

  "init should drop last item from list " in {
    assertResult(List(1,2,3,4)) {
      List.init[Int](List(1,2,3,4,5))
    }
  }

  "product should return product of list " in {
    assertResult(24) {
      List.product(List(1,2,3,4)){ (x:Double, y:Double)=>x*y}
    }
  }

  "product should return 0 for list containing 0" in {
    assertResult(0) {
      List.product(List(1,0,3,4)){ (x:Double, y:Double)=>x*y}
    }
  }

  "length should return length for list " in {
    assertResult(4) {
      List.length(List(1,2,3,4))
      List.length3(List(1,2,3,4))
    }
  }
  "reverse should return reverse of list " in {
    assertResult(List(4,3,2,1)) {
      List.reverse(List(1,2,3,4))
    }
  }

  ".map" when {
    "using map traverse and apply f " in {
      assertResult(List(2,4,6,8)) {
        List.map[Int,Int](List(1,2,3,4))(_*2)
      }
    }
  }

  ".filter" when {
    "Filter out odd ones" in {
      assertResult(List(2,4,6,8)) {
        List.filterViaFlatMap[Int](List(1,2,3,4,5,6,7,8))(_%2==0)
      }
    }
  }

  ".flatMap" when {
    "FlaMap the list to single list" in {
      assertResult(List(1,1,2,2,3,3,4,4)) {
        List.flatMap[Int,Int](List(1,2,3,4))(x=>List(x,x))
      }
    }
  }

  ".hasSubsequence" when {
    "Check for sub sequence in List" in {
      assertResult(true) {
        List.hasSubsequence(List(1,2,3,4), List(2,3))
      }
    }
  }

  "scanRight should produce given List" in {
    List(6, 5, 3, 0) shouldBe List.scanRight(List(1,2,3), 0)(_+_)
  }

}
