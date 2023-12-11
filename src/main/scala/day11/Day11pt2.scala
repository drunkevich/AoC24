package advent
package day11

import util.Advent

object Day11pt2 extends Advent("day11_pt2") {

  override def eval(lines: Seq[String]) = {
    Galaxy.evalSumOfDistancesAfterExpansion(lines, 1000000)
  }
}

