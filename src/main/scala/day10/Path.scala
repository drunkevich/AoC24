package advent
package day10

import scala.annotation.tailrec
import util.*
object Path {

  def calculate(map: Array[Array[Tile]]) = {
    def neighbors(position: Position): Seq[(Direction, Tile)] =
      for {
        direction <- Seq(N, S, E, W)
        newX = position.x + direction.dx
        newY = position.y + direction.dy
        if newY >= 0 && newY < map.length && newX >= 0 && newX < map(newY).length
      } yield (direction, map(newY)(newX))

    val start = (for {
      y <- map.indices
      x <- map(y).indices
      if map(y)(x) == Start
    } yield Position(x, y)).head

    val t = neighbors(start)
    val startNeighbors = neighbors(start).filter {
      case (N, tile) => tile == Vertical || tile == CornerSE || tile == CornerSW
      case (S, tile) => tile == Vertical || tile == CornerNE || tile == CornerNW
      case (E, tile) =>
        tile == Horizontal || tile == CornerNW || tile == CornerSW
      case (W, tile) =>
        tile == Horizontal || tile == CornerNE || tile == CornerSE
    }
    val startNeighbor = startNeighbors.head

    def nextStep(position: Position, previousDirection: Direction): Position = {
      val newDirection = map(position.y)(position.x) match {
        case Pipe(a, b) => if (a == previousDirection) b else a
      }
      position.plus(newDirection)
    }

    @tailrec
    def walk(walked: Seq[Position]): Seq[Position] = {
      val current = walked.head
      val previous = walked.tail.head
      val nextPosition = nextStep(current, Direction.fromTo(current, previous))
      if (map(nextPosition.y)(nextPosition.x) == Start) walked
      else walk(nextPosition +: walked)
    }

    val path = walk(start.plus(startNeighbor._1) :: start :: Nil)
    path
  }
}

object Parser {
  def tile(char: Char): Tile = char match {
    case '.' => Empty
    case 'S' => Start
    case '-' => Horizontal
    case '|' => Vertical
    case 'L' => CornerNE
    case 'J' => CornerNW
    case 'F' => CornerSE
    case '7' => CornerSW
  }
}

sealed trait Tile
object Start extends Tile {
  override def toString: String = "S"
}
object Empty extends Tile {
  override def toString: String = "."
}

case class Pipe(direction1: Direction, direction2: Direction) extends Tile
object Horizontal extends Pipe(E, W) {
  override def toString: String = "-"
}
object Vertical extends Pipe(N, S) {
  override def toString: String = "|"
}
object CornerNE extends Pipe(N, E) {
  override def toString: String = "L"
}
object CornerNW extends Pipe(N, W) {
  override def toString: String = "J"
}
object CornerSE extends Pipe(S, E) {
  override def toString: String = "F"
}
object CornerSW extends Pipe(S, W) {
  override def toString: String = "7"
}
