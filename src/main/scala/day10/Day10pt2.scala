package advent
package day10

import util.Advent
import day09.Util.addRow

import java.nio.channels.Pipe
import scala.::
import scala.annotation.tailrec

object Day10pt2 extends Advent("day10_pt2") {

  override def eval(lines: Seq[String]) = {
    val map = lines.map(_.toCharArray.map(Parser.tile)).toArray

    val path = Path.calculate(map).toArray

    val directionFromSForward = Direction.fromTo(path.last, path.head)
    val directionFromSBack = Direction.fromTo(path.last, path(path.length - 2))
    val sReplacement = (directionFromSForward, directionFromSBack) match {
      case (S, N) | (N, S) => Vertical
      case (E, W) | (W, E) => Horizontal
      case (S, E) | (E, S) => CornerSE
      case (S, W) | (W, S) => CornerSW
      case (N, E) | (E, N) => CornerNE
      case (N, W) | (W, N) => CornerNW
    }

    val pathMap: Array[Array[Tile]] =
      Array.fill(map.length, map(0).length)(Empty)
    path.foreach {
      case Position(x, y) => pathMap(y)(x) = map(y)(x)
    }
    val sPosition = path.last
    pathMap(sPosition.y)(sPosition.x) = sReplacement

    pathMap.map {
      _.foldLeft(Acc(0, "OUT")) {
        case (acc, tile) =>
          tile match {
            case Empty if acc.inside == "IN" =>
              acc.copy(innerCount = acc.innerCount + 1)
            case Empty if acc.inside == "OUT" => acc
            case Horizontal if ((acc.inside == "N") || (acc.inside == "S")) =>
              acc
            case Vertical if acc.inside == "IN"  => acc.copy(inside = "OUT")
            case Vertical if acc.inside == "OUT" => acc.copy(inside = "IN")
            case CornerNE if acc.inside == "IN"  => acc.copy(inside = "S")
            case CornerNE if acc.inside == "OUT" => acc.copy(inside = "N")
            case CornerSE if acc.inside == "IN"  => acc.copy(inside = "N")
            case CornerSE if acc.inside == "OUT" => acc.copy(inside = "S")
            case CornerNW if acc.inside == "S"   => acc.copy(inside = "IN")
            case CornerNW if acc.inside == "N"   => acc.copy(inside = "OUT")
            case CornerSW if acc.inside == "S"   => acc.copy(inside = "OUT")
            case CornerSW if acc.inside == "N"   => acc.copy(inside = "IN")
          }
      }.innerCount
    }.sum
  }

}

case class Acc(innerCount: Int, inside: "IN" | "OUT" | "N" | "S")
