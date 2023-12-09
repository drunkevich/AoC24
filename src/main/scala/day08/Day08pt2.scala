package advent
package day08

import util.Advent

import scala.annotation.tailrec

object Day08pt2 extends Advent("day08_pt2") {
  override def eval(lines: Seq[String]) = {
    val parsedDirections = Parser.directions(lines.head.trim).toArray
    val directionsSize = parsedDirections.length

    val instructions =
      lines.tail.tail.map(Parser.instruction).map(i => i.start -> i).toMap

    @tailrec
    def getPath(breadcrumbs: List[BreadCrumb],
                directionPointer: Int): List[BreadCrumb] = {
      val nextDirectionPointer = (directionPointer + 1) % directionsSize
      val nextDirection = parsedDirections(nextDirectionPointer)
      val currentPoint = breadcrumbs.head.point
      val currentDirection = breadcrumbs.head.direction
      val nextPoint = currentDirection match {
        case Left  => instructions(currentPoint).left
        case Right => instructions(currentPoint).right
      }
      val nextBreadcrumb =
        BreadCrumb(nextPoint, nextDirection, nextDirectionPointer)
      val nextBreadCrumbs = nextBreadcrumb +: breadcrumbs
      if (breadcrumbs.contains(nextBreadcrumb)) {
        nextBreadCrumbs
      } else {
        getPath(nextBreadCrumbs, nextDirectionPointer)
      }
    }

    val cycles: Seq[Long] = instructions.values
      .filter(_.start.isStart)
      .map(instr => {
        val path = getPath(
          List(
            BreadCrumb(
              instr.start,
              parsedDirections.head,
              0
            )
          ),
          0
        )
        val lastNode = path.head
         val lasts = path.reverse.zipWithIndex.filter(_._1 == lastNode).map(_._2)
        (lasts.last - lasts.head).toLong
      }).toSeq
    
    LCM(cycles)
  }

}
