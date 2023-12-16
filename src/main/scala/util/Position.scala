package advent
package util

import scala.annotation.targetName

case class Position(x: Int, y: Int) {
  def plus(direction: Direction): Position = Position(x + direction.dx, y + direction.dy)
  
  def minus(direction: Direction): Position = Position(x - direction.dx, y - direction.dy)

  @targetName("add")
  def +(other: Position): Position = Position(x + other.x, y + other.y)

  def in(box: Position): Boolean = x >= 0 && y >= 0 && x < box.x && y < box.y
}

type Dirr = 0 | 1 | -1
case class Direction(dx: Dirr, dy: Dirr)
object N extends Direction(0, -1) {
  override def toString: String = "N"
}
object S extends Direction(0, 1) {
  override def toString: String = "S"
}
object E extends Direction(1, 0) {
  override def toString: String = "E"
}
object W extends Direction(-1, 0) {
  override def toString: String = "W"
}
object Direction {
  def fromTo(from: Position, to: Position): Direction =
    val dx = to.x - from.x
    val dy = to.y - from.y
    (dx, dy) match
      case (a: Dirr, b: Dirr) =>  Direction(a, b)
}