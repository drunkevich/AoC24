package advent
package day14

import util.*
import day14.Rocks.*

object Day14pt2 extends Advent("day14_pt2") {

  override def eval(lines: Seq[String]) = {
    val roundRocks = findRocks(lines, 'O')
    val squareRocks = findRocks(lines, '#')
    implicit val box: Position = Position(lines.head.length, lines.size)

    val cycles = LazyList.iterate(roundRocks)(cycle(_, squareRocks))

    val loop = cycles.zipWithIndex.find {
      case (rocks, i) => cycles.take(i).contains(rocks)
    }.get

    val loopEnd = loop._2
    val loopStart = cycles.indexOf(loop._1)
    val cycleNeeded = 1000000000L
    val cyclesEqu = (cycleNeeded - loopStart) % (loopEnd - loopStart)

    val finalRocks = cycles(loopStart + cyclesEqu.toInt)
    finalRocks.toSeq
      .map(_.y)
      .map(lines.size - _)
      .sum
  }

}
