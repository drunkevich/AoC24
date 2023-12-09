package advent
package day03

import util.Advent

object Day03pt1 extends Advent("day03_pt1") {
  override def eval(lines: Seq[String]) = {
    val list = lines.toArray
    val parsedNumbers = lines.zipWithIndex.flatMap {
      case (line, lineNum) => NumberPosition.parseNumbers(line, lineNum)
    }
    parsedNumbers
      .filter(parsedNumber => {
        val up = parsedNumber.lineNum - 1 max 0
        val down = parsedNumber.lineNum + 1 min list.length - 1
        val left = parsedNumber.start - 1 max 0
        val right = parsedNumber.end + 1 min list(parsedNumber.lineNum).length - 1
        val nonDigitChars = for {
          i <- up to down
          j <- left to right
          char = list(i)(j)
          if !char.isDigit && char != '.'
        } yield char
        nonDigitChars.nonEmpty
      })
      .map(_.value)
      .sum
  }
}
