package advent
package day9

import util.Advent

import day9.Util.addRow


object Day09pt1 extends Advent("day09_pt1") {

  override def evalSum(line: String) = {
    val input = line.split(" ").map(_.toLong)
    val table: List[Array[Long]] = List(input)


    val diffs = addRow(table)
    diffs.map(_.last).sum
  }

}
