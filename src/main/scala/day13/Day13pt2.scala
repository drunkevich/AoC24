package advent
package day13

import util.Advent
import day13.Utils.*

object Day13pt2 extends Advent("day13_pt2") {

  override def eval(lines: Seq[String]) = {
    val patterns = Utils.groupPatterns(lines)
    patterns.map(evalPattern).sum
  }

  def evalPattern(lines: Seq[String]): Int = {
    val rocks = Utils.findRocks(lines)
    val width = lines.head.length
    val height = lines.length

    val verticalAsyms = for {
      x <- 1 until width
      asym <- findVerticalAsymmetry(rocks, x, width)
    } yield asym
    val horizontalAsyms = for {
      y <- 1 until height
      asym <- findHorizontalAsymmetry(rocks, y, height)
    } yield asym

    val smudge = (verticalAsyms ++ horizontalAsyms).head

    val fixedRocks = if (rocks.contains(smudge)) {
      rocks - smudge
    } else {
      rocks + smudge
    }

    val newVerticalAxis = findVerticalAxis(fixedRocks, width)
    val oldVerticalAxis = findVerticalAxis(rocks, width)
    val verticalAxis = (newVerticalAxis.toSet diff oldVerticalAxis.toSet).toSeq.headOption

    val newHorizontalAxis = findHorizontalAxis(fixedRocks, height)
    val oldHorizontalAxis = findHorizontalAxis(rocks, height)
    val horizontalAxis = (newHorizontalAxis.toSet diff oldHorizontalAxis.toSet).toSeq.headOption

    (verticalAxis, horizontalAxis) match {
      case (Some(x), None) => x
      case (None, Some(y)) => y * 100
    }
  }

}
