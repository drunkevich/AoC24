package advent
package day4

import util.Advent

object Day04pt2 extends Advent("day04_pt2") {

  override def eval(lines: Seq[String]) = {
    val cards = lines.map(Card.parse)
    val copies = Array.fill(cards.size)(1)
    cards.zipWithIndex.foreach {
      case (card, index) =>
        val winingNumbers = card.numbers.count(card.wins.contains)
        val copiesCount = copies(index)
        if (winingNumbers > 0) {
          (1 to winingNumbers).map(index + _).foreach {
            i => copies(i) = copies(i) + copiesCount
          }
        }
    }
    copies.sum.toString
  }
}
