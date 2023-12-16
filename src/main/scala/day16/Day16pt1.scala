package advent
package day16

import util.*

import scala.annotation.targetName

object Day16pt1 extends Advent("day16_pt1") {

  override def eval(lines: Seq[String]) = {
    val box: Position = Position(lines.head.length, lines.length)

    val mirrors = (for {
        (line, y) <- lines.zipWithIndex
        (char, x) <- line.zipWithIndex
        if char != '.'
        } yield (Position(x, y), Mirror(char))).toMap

    val initial = (Position(0, 0).minus(E), E)
    val lights = Mirror.lightStep(Set(initial), Set(initial), mirrors, box)

    val lightPositions = lights.map(_._1)

    lightPositions.size -1
  }
}
