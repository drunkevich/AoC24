package advent
package day2

import util.Advent

object Day02pt2 extends Advent("day02_pt2") {

  override def evalSum(line: String): Long = {
    val cubes = Game.parse(line)
    val fold = cubes.sets.fold(CubesSet(0, 0, 0)) {
      case (CubesSet(r1, g1, b1), CubesSet(r2, g2, b2)) =>
        CubesSet(r1 max r2, g1 max g2, b1 max b2)
    }
    fold.red * fold.green * fold.blue
  }
}
