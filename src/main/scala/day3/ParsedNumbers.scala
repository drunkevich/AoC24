package advent
package day3

import scala.annotation.tailrec

case class NumberPosition(lineNum: Int, start: Int, end: Int, string: String) {
  val value = string.toInt
}
object NumberPosition {
  def parseNumbers(line: String, lineNum: Int): Seq[NumberPosition] =
    parseNumbersRec(0, line, lineNum, Seq.empty, None)

  @tailrec
  def parseNumbersRec(index: Int,
                      line: String,
                      lineNum: Int,
                      parsed: Seq[NumberPosition],
                      draft: Option[NumberPosition]): Seq[NumberPosition] = {
    if (index >= line.length) {
      parsed ++ draft.toList
    } else {
      val char = line.charAt(index)
      if (char.isDigit) {
        val newDraft = if (draft.isEmpty) {
          NumberPosition(lineNum, index, index, char.toString)
        } else {
          draft.get.copy(end = index, string = draft.get.string + char)
        }
        parseNumbersRec(index + 1, line, lineNum, parsed, Some(newDraft))
      } else {
        parseNumbersRec(index + 1, line, lineNum, parsed ++ draft.toList, None)
      }
    }
  }
}
