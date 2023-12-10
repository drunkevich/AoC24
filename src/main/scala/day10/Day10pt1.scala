package advent
package day10

import util.Advent

import scala.annotation.tailrec


object Day10pt1 extends Advent("day10_pt1") {

  override def eval(lines: Seq[String]) = {
    val map = lines.map(_.toCharArray.map(Parser.tile)).toArray

    val path = Path.calculate(map)
    path.length / 2
  }

}
