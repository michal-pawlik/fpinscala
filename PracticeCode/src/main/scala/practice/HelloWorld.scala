package practice

object HelloWorld extends App {

  println("Hello World")

  val arr: Array[Any] = Array(Array(1, 2, Array(3)), 4)
  flattenArray(arr).toList.foreach(println)

  def flattenArray[A](arr: Array[A]): Array[Int] = {
    arr.flatMap {
      case s: Int      => Array(s)
      case a: Array[_] => flattenArray(a)
    }
  }
}
