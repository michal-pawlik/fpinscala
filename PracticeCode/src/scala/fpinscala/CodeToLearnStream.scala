package fpinscala

import fpinscala.laziness.Stream
import fpinscala.laziness.Stream._

object CodeToLearnStream extends App {

  def getInt = 1
  val dummyStream: Stream[() => Int] = Stream.cons(() => 1, Stream.cons(() => 2, empty))
  assert(dummyStream.toListRecursive.head() == 1)
  assert(dummyStream.toList.head() == 1)
  assert(dummyStream.headOption2.get() == 1)

  dummyStream.take(2).toList.map(x => println("take " + x()))
  dummyStream.takeWhile(x => x() % 2 != 0).toList.map(x => println("takeWhile " + x()))

//  dummyStream.foldRight(() => 0)((a,b) => {println(a); () => 1})

  println(dummyStream.foldRight(0)((a,acc) => { a() + acc}))

  println(dummyStream.foldRight(() => 0)((a,acc) => { () => a() + acc()})())

  // 2 is not computed :)
  println(dummyStream.exists(x => { println("exists " + x()); x() % 2 != 0}))

  println(Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList)

  println(ones.take(5).toList)
  println(ones.map(_+1).exists(_%2==0))
  println(ones.forAll(_ != 1))

  println(Stream.constant2(1).take(5).toList)

  println(Stream.fibs.take(8).toList)

  println(Stream.unfold(0)(x => Some((x, x+1))).take(5).toList)
  println(Stream.unfold((0,1))(x => Some((x._1, (x._2, x._1 + x._2)))).take(8).toList)
  println(Stream.unfold((0,1))(x => x match { case (f0,f1) => Some((f0, (f1, f0 + f1)))}).take(8).toList)

  println(Stream.unfoldViaFold((0,1))(x => Some((x._1, (x._2, x._1 + x._2)))).take(8).toList)

  println(Stream.fibsViaUnfold.take(5).toList)

  dummyStream.tails.toList.foreach(x => {x.toList.foreach(x => print(x())); println})
  dummyStream.tails2.toList.foreach(x => {x.toList.foreach(x => print(x())); println})

}
