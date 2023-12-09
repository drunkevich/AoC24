package advent
package day06

import util.Advent

object Day06pt1 extends Advent("day06_pt1") {
  override def eval(lines: Seq[String]) = {
    val times = Parser.parse(Parser.timePattern)(lines(0))
    val distances = Parser.parse(Parser.distancePattern)(lines(1))
    val races = times zip distances map {
      case (time, distance) => Race(time, distance)
    }
    val winVariants = races.map(_.winsCount)
    winVariants.product
  }
}
