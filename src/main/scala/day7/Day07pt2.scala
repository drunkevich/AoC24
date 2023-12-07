package advent
package day7

import util.Advent

object Day07pt2 extends Advent("day07_pt2") {
  override def eval(lines: Seq[String]) = {
    lines
      .map(HandAndBid.parse)
      .sorted
      .zipWithIndex
      .map {
        case (handAndBid, index) => handAndBid.bid * (index + 1)
      }
      .sum
      .toString
  }


  sealed abstract class Card(val name: String, val value: Int)
    extends Ordered[Card] {
    override def compare(that: Card): Int = value - that.value
  }

  case object A extends Card("A", 14)

  case object K extends Card("K", 13)

  case object Q extends Card("Q", 12)

  case object J extends Card("J", 1)

  case object T extends Card("T", 10)

  case class N(i: Int) extends Card(i.toString, i)

  sealed abstract class HandType(val value: Int) extends Ordered[HandType] {
    override def compare(that: HandType): Int = value - that.value
  }

  case object HighCard extends HandType(1)

  case object OnePair extends HandType(2)

  case object TwoPairs extends HandType(3)

  case object ThreeOfAKind extends HandType(4)

  case object FullHouse extends HandType(5)

  case object FourOfAKind extends HandType(6)

  case object FiveOfAKind extends HandType(7)

  case class Hand(cards: Seq[Card]) extends Ordered[Hand] {
    val jCount = cards.count(_ == J)
    val sortedCardsWithoutJ = cards.filter(_ != J).groupBy(_.value).mapValues(_.size).values.toSeq.sorted.reverse
    val sortedCardsWithOptimalWildcard = sortedCardsWithoutJ match {
      case head :: tail => head + jCount :: tail
      case Nil => Seq(jCount)
    }
    val handType = sortedCardsWithOptimalWildcard match {
        case Seq(1, 1, 1, 1, 1) => HighCard
        case Seq(2, 1, 1, 1) => OnePair
        case Seq(2, 2, 1) => TwoPairs
        case Seq(3, 1, 1) => ThreeOfAKind
        case Seq(3, 2) => FullHouse
        case Seq(4, 1) => FourOfAKind
        case Seq(5) => FiveOfAKind
      }

    override def compare(that: Hand) = {
      if (this.handType == that.handType) {
        this.cards zip that.cards map {
          case (a, b) => a.compare(b)
        } find (_ != 0) getOrElse 0
      } else {
        this.handType.compare(that.handType)
      }
    }
  }

  case class HandAndBid(hand: Hand, bid: Int) extends Ordered[HandAndBid] {
    override def compare(that: HandAndBid) = this.hand.compare(that.hand)
  }

  object HandAndBid {
    def parse(line: String) = {
      val cards = line.split(" ").head map {
        case 'A' => A
        case 'K' => K
        case 'Q' => Q
        case 'J' => J
        case 'T' => T
        case n => N(n.toString.toInt)
      }
      val bid = line.split(" ").last.trim.toInt
      HandAndBid(Hand(cards), bid)
    }
  }
}
