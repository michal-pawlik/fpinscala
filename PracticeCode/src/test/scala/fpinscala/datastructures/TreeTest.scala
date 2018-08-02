package fpinscala.datastructures

import org.scalatest.WordSpec

class TreeTest extends WordSpec {

  "Size" when {
    "Calculate Tree size" in {
      assertResult(5) {
        Tree.size(Branch(Branch(Leaf(4), Leaf(5)), Leaf(6)))
      }
    }
  }

  "Maximum" when {
    "Calculate max value in Tree" in {
      assertResult(6) {
        Tree.maximum(Branch(Branch(Leaf(4), Leaf(5)), Leaf(6)))
      }
    }
  }


  "Depth" when {
    "Calculate depth value pf Tree" in {
      assertResult(3) {
        Tree.depth(
          Branch(
            Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))), Branch(Leaf(6), Leaf(7))
          )
        )
      }
    }
  }

  "Map" when {
    "Transform Tree using map" in {
      assertResult(Branch(Branch(Leaf(8), Leaf(10)), Leaf(12))) {
        Tree.map[Int, Int](Branch(Branch(Leaf(4), Leaf(5)), Leaf(6)))(_ * 2)
      }
    }
  }

  "Fold" when {
    "Use Fold for various func in Tree" when {
      "For Sum" in {
        assertResult(18) {
          Tree.fold[Int, Int](Branch(Branch(Leaf(4), Leaf(5)), Leaf(6)))(_+1)(_+_)
        }
      }
    }
  }

}
