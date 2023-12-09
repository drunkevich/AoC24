package advent
package day08

import util.Advent

import scala.annotation.tailrec

object Day08pt1 extends Advent("day08_pt1") {
  override def eval(lines: Seq[String]) = {
    val parsedDirections = Parser.directions(lines.head.trim)

    val instructions =
      lines.tail.tail.map(Parser.instruction).map(i => i.start -> i).toMap

    @tailrec
    def nextStep(steps: Int,
                 currentPoint: Point,
                 directions: Seq[Direction]): Int = {
      val infiniteDirections =
        if (directions.isEmpty) parsedDirections else directions
      val nextDirection = infiniteDirections.head
      val nextPoint = nextDirection match {
        case Left  => instructions(currentPoint).left
        case Right => instructions(currentPoint).right
      }
      if (nextPoint == Point("ZZZ")) (steps + 1)
      else nextStep(steps + 1, nextPoint, infiniteDirections.tail)
    }

    nextStep(0, Point("AAA"), parsedDirections)
  }

}
