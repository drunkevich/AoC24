package advent
package day17

import util.*

object Day17pt2 extends Advent("day17_pt2") {

  override def eval(lines: Seq[String]) = Path.eval(lines, 4, 10)
}
