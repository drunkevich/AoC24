package advent
package day14

import util.Advent

import day14.Rocks.*

object Day14pt1 extends Advent("day14_pt1") {

  override def eval(lines: Seq[String]) = {
    val roundRocks = findRocks(lines, 'O')
    val squareRocks = findRocks(lines, '#')

    val movedRocks = moveTillTheEnd(North, roundRocks, squareRocks)(Point(lines.head.length, lines.size))

    val h = lines.size
    movedRocks
      .toSeq
      .map(_.y)
      .map(h - _)
      .sum
  }

}
