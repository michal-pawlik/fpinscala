//package fpinscala
//
//import fpinscala.datastructures.{Branch, Leaf, Tree}
//
//object CodeToLearnTree extends App {
//
//  //        B
//  //    B      L-6
//  // L-4 R-5
//
//  val t: Tree[Int] = Branch(Branch(Leaf(4), Leaf(5)), Leaf(6))
//
//  assert(Tree.size(t) == 5)
//  assert(Tree.size2(t) == 5)
//
//  assert(Tree.maximum(t) == 6)
//
//  assert(Tree.depth(t) == 2)
//
//  assert(Tree.map(t)(i => i * i) == Branch(Branch(Leaf(16),Leaf(25)),Leaf(36)))
//
//  assert(Tree.fold(t)(i => i)(_*_) == 120)
//
//  assert(Tree.sizeViaFold(t) == 5)
//
//  assert(Tree.maximumViaFold(t) == 6)
//
//  assert(Tree.depthViaFold(t) == 2)
//}
