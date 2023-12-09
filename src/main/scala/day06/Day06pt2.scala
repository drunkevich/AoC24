package advent
package day06

import util.Advent

object Day06pt2 extends Advent("day06_pt2") {
  override def eval(lines: Seq[String]) = {
    val time = Parser.parseWholeLine(Parser.timePattern)(lines(0))
    val distance = Parser.parseWholeLine(Parser.distancePattern)(lines(1))
    Race(time, distance).winsCount
  }
}
