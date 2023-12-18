package advent
package day17

import util.*

object Day17pt1 extends Advent("day17_pt1") {

  override def eval(lines: Seq[String]) = Path.eval(lines, 1, 3)
}
