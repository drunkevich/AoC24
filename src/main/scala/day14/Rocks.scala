package advent
package day14

import scala.annotation.{tailrec, targetName}

case class Point(x: Int, y: Int) {
  @targetName("add")
  def +(other: Point): Point = Point(x + other.x, y + other.y)

  def in(box: Point): Boolean = x >= 0 && y >= 0 && x < box.x && y < box.y
}

object North extends Point(0, -1)
object South extends Point(0, 1)
object West extends Point(-1, 0)
object East extends Point(1, 0)
object Rocks {

  def move(direction: Point, roundRocks: Set[Point], squareRocks: Set[Point])(
    implicit box: Point
  ): Set[Point] = {
    roundRocks.map { point =>
      val newPoint = point + direction
      if (newPoint.in(box) &&
          !squareRocks.contains(newPoint) &&
          !roundRocks.contains(newPoint))
        newPoint
      else point
    }
  }

  @tailrec
  def moveTillTheEnd(
    direction: Point,
    roundRocks: Set[Point],
    squareRocks: Set[Point]
  )(implicit box: Point): Set[Point] = {
    val moved = move(direction, roundRocks, squareRocks)
    if (moved == roundRocks) moved
    else moveTillTheEnd(direction, moved, squareRocks)
  }

  def cycle(roundRocks: Set[Point],
            squareRocks: Set[Point])(implicit box: Point): Set[Point] = {
    val movedNorth = moveTillTheEnd(North, roundRocks, squareRocks)
    val movedWest = moveTillTheEnd(West, movedNorth, squareRocks)
    val movedSouth = moveTillTheEnd(South, movedWest, squareRocks)
    val movedEast = moveTillTheEnd(East, movedSouth, squareRocks)
    movedEast
  }

  def findRocks(lines: Seq[String], form: Char): Set[Point] =
    (for {
      (line, y) <- lines.zipWithIndex
      (char, x) <- line.zipWithIndex
      if char == form
    } yield Point(x, y)).toSet

}
