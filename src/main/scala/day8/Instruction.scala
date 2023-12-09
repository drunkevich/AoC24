package advent
package day8

import scala.annotation.tailrec

sealed trait Direction
case object Left extends Direction
case object Right extends Direction

case class Point(label: String) {
  val isStart = label.endsWith("A")
  val isEnd = label.endsWith("Z")
}

case class Instruction(start: Point, left: Point, right: Point)

case class BreadCrumb(point: Point,
                      direction: Direction,
                      instructionLength: Int)

object Parser {
  def directions(s: String): Seq[Direction] = s map {
    case 'L' => Left
    case 'R' => Right
  }

  val instructionPattern = """^(.{3}) = \((.{3}), (.{3})\)$""".r

  def instruction(string: String): Instruction = string.trim match {
    case instructionPattern(start, left, right) =>
      Instruction(Point(start), Point(left), Point(right))
  }
}

object LCM {
  def apply(numbers: Seq[Long]): Long = {
    numbers.reduce(apply);
  }

  def apply(a: Long, b: Long): Long = {
    a * b / gcd(a, b)
  }

  @tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0) a.abs else gcd(b, a % b)
}
