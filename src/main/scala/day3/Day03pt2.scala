package advent
package day3

import util.Advent

object Day03pt2 extends Advent("day03_pt2") {
  override def eval(lines: Seq[String]) = {
    val list = lines.toArray
    val parsedNumbers = lines.zipWithIndex.flatMap {
      case (line, lineNum) => NumberPosition.parseNumbers(line, lineNum)
    }
    val gears = parsedNumbers
      .flatMap(parsedNumber => {
        val up = parsedNumber.lineNum - 1 max 0
        val down = parsedNumber.lineNum + 1 min list.length - 1
        val left = parsedNumber.start - 1 max 0
        val right = parsedNumber.end + 1 min list(parsedNumber.lineNum).length - 1
        for {
          i <- up to down
          j <- left to right
          char = list(i)(j)
          if char == '*'
        } yield GearNeighbor(i, j, parsedNumber)
      })
    gears
      .groupBy(g => (g.i, g.j))
      .filter(_._2.length == 2)
      .values
      .map(gears => gears.head.number.value * gears.last.number.value)
      .sum
      .toString
  }
}

case class GearNeighbor(i: Int, j: Int, number: NumberPosition)
