package scala.fpinscala

import scala.fpinscala.datastructures.{Cons, List}


object CodeToLearn2 extends App {


  println(List.length(List(1,2,4,5)))

  println(List.reverse(List(1,2,3,4,5)))

  println(List.foldRightViaFoldLeft(List(1,2,3,4,5), List[Int]())(Cons(_,_)))

}
