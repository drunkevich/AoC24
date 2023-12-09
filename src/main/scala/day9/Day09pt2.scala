package advent
package day9

import util.Advent

import advent.day9.Util.addRow


object Day09pt2 extends Advent("day09_pt2") {

  override def evalSum(line: String) = {
    val input = line.split(" ").map(_.toLong)
    val table: List[Array[Long]] = List(input)

    val diffs = addRow(table)
    val signs = diffs.indices.map(i => if (i % 2 == 0) 1 else -1)
    diffs.map(_.head).zip(signs).map { case (a, b) => a * b }.sum
  }

}
