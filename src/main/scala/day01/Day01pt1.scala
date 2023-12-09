package advent
package day01

import util.Advent

object Day01pt1 extends Advent("day01_pt1") {
  override def evalSum(line: String): Long = {
    val digits = line.filter(_.isDigit)
    val firstDigit = digits.substring(0, 1)
    val lastDigit = digits.substring(digits.length - 1)
    val sum = (firstDigit + lastDigit)
    sum.toString.toLong
  }
}
