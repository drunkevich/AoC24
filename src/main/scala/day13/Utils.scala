package advent
package day13

object Utils {
  def groupPatterns(lines: Seq[String]): Seq[Seq[String]] = {
    val pattern = lines.takeWhile(_.nonEmpty)
    if (pattern.isEmpty) {
      Seq.empty
    } else {
      val tail = lines.dropWhile(_.nonEmpty).drop(1)
      pattern +: groupPatterns(tail)
    }
  }

  def findRocks(lines: Seq[String]): Set[(Int, Int)] =
    (for {
      (line, y) <- lines.zipWithIndex
      (char, x) <- line.zipWithIndex
      if char == '#'
    } yield (x, y)).toSet

  def checkVerticalSymmetry(rocks: Set[(Int, Int)],
                            axis: Int,
                            width: Int): Boolean = {
    val mirrorWidth = axis min (width - axis)
    val left = rocks.filter(_._1 < axis).filter(_._1 >= axis - mirrorWidth)
    val right = rocks.filter(_._1 >= axis).filter(_._1 < axis + mirrorWidth)
    val mirror = left.map { case (x, y) => (2 * axis - 1 - x, y) }
    right == mirror
  }

  def checkHorizontalSymmetry(rocks: Set[(Int, Int)],
                              axis: Int,
                              height: Int): Boolean = {
    val mirrorRocks = rocks.map(_.swap)
    checkVerticalSymmetry(mirrorRocks, axis, height)
  }

  def findVerticalAsymmetry(rocks: Set[(Int, Int)],
                            axis: Int,
                            width: Int): Option[(Int, Int)] = {
    val mirrorWidth = axis min (width - axis)
    val left = rocks.filter(_._1 < axis).filter(_._1 >= axis - mirrorWidth)
    val right = rocks.filter(_._1 >= axis).filter(_._1 < axis + mirrorWidth)
    val mirror = left.map { case (x, y) => (2 * axis - 1 - x, y) }
    val rightDiff = (right diff mirror).toSeq
    val leftDiff = (mirror diff right).toSeq
    (rightDiff, leftDiff) match {
      case (Nil, Seq((x, y))) => {
        Some((2 * axis - 1 - x, y))
      }
      case (Seq((x, y)), Nil) => {
        Some((x, y))
      }
      case _             => None
    }
  }

  def findHorizontalAsymmetry(rocks: Set[(Int, Int)],
                              axis: Int,
                              height: Int): Option[(Int, Int)] = {
    val mirrorRocks = rocks.map(_.swap)
    findVerticalAsymmetry(mirrorRocks, axis, height).map(_.swap)
  }

  def findVerticalAxis(rocks: Set[(Int, Int)], width: Int): Seq[Int] = for {
      x <- 1 until width
      if checkVerticalSymmetry(rocks, x, width)
    } yield x
  
  def findHorizontalAxis(rocks: Set[(Int, Int)], height: Int): Seq[Int] = for {
      y <- 1 until height
      if checkHorizontalSymmetry(rocks, y, height)
    } yield y
  
}
