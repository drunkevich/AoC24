package advent
package day1

object Day01pt1 extends App {
  val sum = input.full.trim
    .split('\n')
    .map(line => {
      val digits = line.filter(_.isDigit)
      val firstDigit = digits.substring(0, 1)
      val lastDigit = digits.substring(digits.length - 1)
      val sum = (firstDigit + lastDigit)
      val result = sum.toString.toInt
      result
    })
    .sum
  println(sum)

}
