package advent
package day18

import util.*

object Day18pt1 extends Advent("day18_pt1") {

  override def eval(lines: Seq[String]) = {
    val plan = lines.map { line =>
      val split = line.split(" ")
      val direction = split(0) match {
        case "U" => N
        case "D" => S
        case "L" => W
        case "R" => E
      }
      val distance = split(1).toInt
      (direction, distance)
    }.toArray

    val outer = plan
      .foldLeft(Array(Position(0, 0))) {
        case (acc, (direction, distance)) =>
          acc ++ (1 to distance).map { i =>
            acc.last + direction * i
          }.toArray
      }
      .drop(1)
      .toSet

    (outer.map(_.y).min to outer.map(_.y).max).foreach { i =>
      (outer.map(_.x).min to outer.map(_.x).max).foreach { j =>
        if (outer.contains(Position(j, i))) {
          print("#")
        } else {
          print(".")
        }
      }
      println()
    }
    println()
    println()
    println()


    val minX = outer.map(_.x).min
    val minY = outer.map(_.y).min
    val normalizedOuter =
      outer.map(p => Position(p.x - minX + 1, p.y - minY + 1))

    val box =
      Position(normalizedOuter.map(_.x).max + 2, normalizedOuter.map(_.y).max + 2)

    val outsiders = scala.collection.mutable.Set.empty[Position]
    val newOutsiders = scala.collection.mutable.Set(Position(0, 0))

    while (newOutsiders.nonEmpty) {
      outsiders ++= newOutsiders
      val nextGen = newOutsiders
        .flatMap(p => Set(p plus N, p plus S, p plus E, p plus W))
        .filter(_.in(box))
        .diff(outsiders)
        .diff(normalizedOuter)
      newOutsiders.clear()
      newOutsiders ++= nextGen

      (0 until box.y).foreach { i =>
        (0 until box.x).foreach { j =>
          if (normalizedOuter.contains(Position(j, i))) {
            print("#")
          } else if (outsiders.contains(Position(j, i))) {
            print("0")
          } else {
            print(".")
          }
        }
        println()
      }
      println()
    }

    (0 until box.y).foreach { i =>
      (0 until box.x).foreach { j =>
        if (normalizedOuter.contains(Position(j, i))) {
          print("#")
        } else if (outsiders.contains(Position(j, i))) {
          print("0")
        } else {
          print(".")
        }
      }
      println()
    }

    (box.x) * (box.y) - outsiders.size
  }
}
