package advent
package day18

import util.{Advent, E, N, Position, S, W}

object Day18pt2 extends Advent("day18_pt2") {

  override def eval(lines: Seq[String]) = {
    val plan = lines.map { line =>
      val code = line.split(" ").last.drop(2).dropRight(1)
      val direction = code.last match {
        case '0' => E
        case '1' => S
        case '2' => W
        case '3' => N
      }
      val distance = Integer.parseInt(code.dropRight(1), 16)
      (direction, distance)
    }.toArray

    val outer = plan
      .foldLeft(Array(Position(0, 0))) {
        case (acc, (direction, distance)) =>
          acc ++ (1 to distance).map { i =>
            acc.last + direction * i
          }.toArray
      }
    
    outer.dropRight(1).zip(outer.drop(1)).map { case (p1, p2) =>
      val p3 = p2 - p1
      
    }
  }

}
