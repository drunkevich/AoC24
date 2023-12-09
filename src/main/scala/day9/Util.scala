package advent
package day9

import scala.annotation.tailrec

object Util {

  @tailrec
  def addRow(table: List[Array[Long]]): List[Array[Long]] = {
    val lastRow = table.last
    if (lastRow.forall(_ == 0)) {
      return table
    }
    val newRow = lastRow.tail.zip(lastRow).map { case (a, b) => a - b }
    addRow(table :+ newRow)
  }
}
