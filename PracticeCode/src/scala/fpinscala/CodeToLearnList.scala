package scala.fpinscala

import scala.fpinscala.datastructures.{Cons, List}


object CodeToLearnList extends App {


  println(List.length(List(1,2,4,5)))

  println(List.reverse(List(1,2,3,4,5)))

  println(List.foldRightViaFoldLeft(List(1,2,3,4,5), List[Int]())(Cons(_,_)))


  println(List.flatMap(List(1,2,3,4,5))( i => List(i,i)))
  println(List.flatMap2(List(1,2,3,4,5))( i => List(i,i)))
  println(List.flatMap3(List(1,2,3,4,5))( i => List(i,i)))

  println(List.filterViaFlatMap(List(1,2,3,4,5))( _ % 2 == 0))

  println(List.addPairwise(List(2,3), List(4,5,6)))
  println(List.addPairwise2(scala.fpinscala.datastructures.Nil: List[Int], List(4,5,6)))
  println(List.addPairwise2(List(10,20), List(4,5,6)))


  println(List.startsWith(List(1,2,3,4,5,6), List(1,2)))
  println(List.startsWith(List(1,2,3,4,5,6), List(2,3)))
  println(List.startsWith(List(1,2,3,4,5,6), List(2,3,4)))

  println(List.hasSubsequence(List(1,2,3,4,5,6), List(1,2)))
  println(List.hasSubsequence(List(1,2,3,4,5,6), List(2,3)))
  println(List.hasSubsequence(List(1,2,3,4,5,6), List(2,3,4)))
  println(List.hasSubsequence(List(1,2,3,4,5,6), List(2,3,4,5,6)))
  println(List.hasSubsequence(List(1,2,3,4,5,6), List(2,3,4,5,6,7)))

}
