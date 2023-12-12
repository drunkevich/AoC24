package advent
package day12

import util.Advent

object Day12pt2 extends Advent("day12_pt2") {

  override def evalSum(line: String) = {
    val data = Data.parse(line)
    val newSprings = data.springs ++ List(Unknown) ++ data.springs ++ List(Unknown) ++ data.springs ++ List(Unknown) ++ data.springs ++ List(Unknown) ++ data.springs
    val newControlSum = data.controlSum ++ data.controlSum ++ data.controlSum ++ data.controlSum ++ data.controlSum
    val newData = Data(newSprings, newControlSum)
    Solution(newData)
  }
}