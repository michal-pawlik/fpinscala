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

}
