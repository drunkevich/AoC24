package advent
package day06

import scala.util.matching.Regex

case class Race(time: Long, distance: Long) {
  def winsCount = {
    val discr = time * time - 4 * distance
    val x1 = Math.ceil((time - Math.sqrt(discr)) / 2).toInt
    val x2 = Math.floor((time + Math.sqrt(discr)) / 2).toInt
    val d1 = if((x1 * (time - x1)) == distance) 1 else 0
    val d2 = if((x2 * (time - x2)) == distance) 1 else 0
    val r = x2 - x1 - d1 - d2 + 1
    r
  }
}

object Parser {
  val timePattern = """^Time:(.*)$""".r
  val distancePattern = """^Distance:(.*)$""".r

  def parse(pattern: Regex)(string: String): Seq[Long] = {
    string match {
      case pattern(ids) =>
        ids.split(" ").filter(_.nonEmpty).map(_.toLong)
    }
  }

  def parseWholeLine(pattern: Regex)(string: String): Long = {
    string match {
      case pattern(ids) =>
        ids.filter(_.toString != " ").toLong
    }
  }
}
