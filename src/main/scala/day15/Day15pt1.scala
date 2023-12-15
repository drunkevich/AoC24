package advent
package day15

import util.Advent

object Day15pt1 extends Advent("day15_pt1") {

  override def evalSum(line: String): Long = {
      line.split(",").map(toHash).sum
  }

  def toHash(str: String): Long = {
    str.toCharArray.foldLeft(0L)((acc, c) => (acc + c) * 17 % 256)
  }
}
