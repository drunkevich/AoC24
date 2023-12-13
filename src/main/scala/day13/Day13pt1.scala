package advent
package day13

import util.Advent

import day13.Utils.*

object Day13pt1 extends Advent("day13_pt1") {

  override def eval(lines: Seq[String]) = {
    val patterns = groupPatterns(lines)
    patterns.map(evalPattern).sum
  }

  def evalPattern(lines: Seq[String]): Int = {
    val rocks = findRocks(lines)
    val width = lines.head.length
    val height = lines.length

    val vertical = findVerticalAxis(rocks, width).headOption
    val horizontal = findHorizontalAxis(rocks, height).headOption

    (vertical, horizontal) match {
      case (Some(x), None) => x
      case (None, Some(y)) => y * 100
    }
  }

}
