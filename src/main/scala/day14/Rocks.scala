package advent
package day14

import util.*

import scala.annotation.{tailrec, targetName}

object Rocks {

  def move(direction: Direction, roundRocks: Set[Position], squareRocks: Set[Position])(
    implicit box: Position
  ): Set[Position] = {
    roundRocks.map { point =>
      val newPoint = point.plus(direction)
      if (newPoint.in(box) &&
          !squareRocks.contains(newPoint) &&
          !roundRocks.contains(newPoint))
        newPoint
      else point
    }
  }

  @tailrec
  def moveTillTheEnd(
                      direction: Direction,
                      roundRocks: Set[Position],
                      squareRocks: Set[Position]
  )(implicit box: Position): Set[Position] = {
    val moved = move(direction, roundRocks, squareRocks)
    if (moved == roundRocks) moved
    else moveTillTheEnd(direction, moved, squareRocks)
  }

  def cycle(roundRocks: Set[Position],
            squareRocks: Set[Position])(implicit box: Position): Set[Position] = {
    val movedNorth = moveTillTheEnd(N, roundRocks, squareRocks)
    val movedWest = moveTillTheEnd(W, movedNorth, squareRocks)
    val movedSouth = moveTillTheEnd(S, movedWest, squareRocks)
    val movedEast = moveTillTheEnd(E, movedSouth, squareRocks)
    movedEast
  }

  def findRocks(lines: Seq[String], form: Char): Set[Position] =
    (for {
      (line, y) <- lines.zipWithIndex
      (char, x) <- line.zipWithIndex
      if char == form
    } yield Position(x, y)).toSet

}
