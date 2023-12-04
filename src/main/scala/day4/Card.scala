package advent
package day4

case class Card(id: Int, wins: Set[Int], numbers: Seq[Int])

object Card {
  val gamePattern = """^Card(.*):(.*)\|(.*)$""".r

  def parseInts(string: String): Seq[Int] = {
    string.split(" ").filter(!_.isBlank).map(_.toInt)
  }

  def parse(string: String): Card = {
    string match {
      case gamePattern(id, wins, numbers) => Card(id.trim.toInt, parseInts(wins).toSet, parseInts(numbers))
    }
  }
}
