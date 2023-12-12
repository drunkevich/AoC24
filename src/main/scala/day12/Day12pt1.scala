package advent
package day12

import util.Advent


object Day12pt1 extends Advent("day12_pt1") {

  override def evalSum(line: String) = {
    val data = Data.parse(line)
    Solution(data)
  }
}
