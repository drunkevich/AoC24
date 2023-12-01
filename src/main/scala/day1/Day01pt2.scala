package advent
package day1

object Day01pt2 extends App {

  val digitStrings = Seq(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )

  def findDigits(s: String): Seq[Int] = {
    (0 until s.length).map(s.substring).flatMap { str =>
      if (str.head.isDigit) {
        val h = str.head.toString.toInt
        Some(h)
      } else {
        digitStrings
          .map {
            case (digitString, digit) =>
              if (str.startsWith(digitString)) {
                Some(digit)
              } else {
                None
              }
          }
          .find(_.isDefined)
          .flatten
      }
    }
  }

  val sum = input.full.trim
    .split('\n')
    .map(line => {
      val digits = findDigits(line)
      val firstDigit = digits.head.toString
      val lastDigit = digits.last.toString
      val sum = (firstDigit + lastDigit)
      val result = sum.toString.toInt
      println(result)
      result
    })
    .sum
  println(sum)
}