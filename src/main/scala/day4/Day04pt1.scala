package advent
package day4

import util.Advent

object Day04pt1 extends Advent("day04_pt1") {

  override def evalSum(line: String) = {
    val card = Card.parse(line)
    val winingNumbers = card.numbers.count(card.wins.contains)
    if (winingNumbers == 0) 0 else Math.pow(2, winingNumbers - 1).toInt
  }
}
