package advent
package day16

import util.*

object Day16pt2 extends Advent("day16_pt2") {

  override def eval(lines: Seq[String]) = {
    val box: Position = Position(lines.head.length, lines.length)

    val mirrors = (for {
        (line, y) <- lines.zipWithIndex
        (char, x) <- line.zipWithIndex
        if char != '.'
        } yield (Position(x, y), Mirror(char))).toMap

    val initials: Seq[(Position, Direction)] = (0 until box.x).map(x => (Position(x, 0).minus(S), S)) ++
      (0 until box.x).map(x => (Position(x, box.y - 1).minus(N), N)) ++
      (0 until box.y).map(y => (Position(0, y).minus(E), E)) ++
      (0 until box.y).map(y => (Position(box.x - 1, y).minus(W), W))


    initials.map(initial => {
      val lights = Mirror.lightStep(Set(initial), Set(initial), mirrors, box)
      lights.map(_._1).size
    }).max - 1

  }
}
