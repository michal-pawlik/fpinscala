package fpinscala

import fpinscala.errorhandling._

object CodeToLearnOption extends App {

  val lsOption: Option[List[Int]] = Some(List(1,2,3,4))
  println(lsOption.map(i => i  + "" + i))


  val intOption: Option[Int] = Some(4)
  println(intOption.map(i => i * i))


  def parseInsuranceRateQuote(
                               age: String,
                               numberOfSpeedingTickets: String): Option[Int] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    Option.map2(optAge, optTickets)(parseInsuranceRateQuote)
  }

  def parseInsuranceRateQuote(
                               age: Int,
                               numberOfSpeedingTickets: Int): Int = {
    age + numberOfSpeedingTickets
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  println(parseInsuranceRateQuote("123", "123"))
  println(parseInsuranceRateQuote("123", "12asd3"))
}
