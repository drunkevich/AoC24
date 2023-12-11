package advent
package day11

import util.Advent

object Day11pt1 extends Advent("day11_pt1") {

  override def eval(lines: Seq[String]) = Galaxy.evalSumOfDistancesAfterExpansion(lines, 2)
}

