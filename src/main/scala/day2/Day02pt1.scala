package advent
package day2

import util.Advent

object Day02pt1 extends Advent("day02_pt1") {

  override def eval(lines: Seq[String]) = {
    val red = 12
    val green = 13
    val blue = 14
    lines
      .map(Game.parse)
      .filter(_.sets.forall(turn => {
        turn.red <= red && turn.blue <= blue && turn.green <= green
      }))
      .map(_.id)
      .sum
      .toString
  }
}
