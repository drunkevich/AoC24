package advent
package day15

import util.Advent

object Day15pt2 extends Advent("day15_pt2") {

  override def evalSum(line: String): Long = {
    val boxes = line.split(",").map(toOp).groupBy(l => toHash(l.label))
    val lens = boxes
      .mapValues(
        _.foldLeft(List.empty[Lens])(
          (acc: List[Lens], op) =>
            op match {
              case Add(lens) => {
                val pos = acc.zipWithIndex.find(_._1.label == lens.label)
                pos
                  .map {
                    case (l, i) => acc.take(i) ++ List(lens) ++ acc.drop(i + 1)
                  }
                  .getOrElse(acc :+ lens)
              }
              case Remove(l) => acc.filter(_.label != l)
          }
        )
      )
    lens.map {
      case (k, v) => {
        val lensSum = v.zipWithIndex.map {
          case (lens, i) => lens.value * (i + 1)
        }.sum
        lensSum * (k + 1)
      }
    }.sum
  }

  def toOp(str: String): Op = {
    if (str.contains("=")) {
      str.split("=") match {
        case Array(label, value) => Add(Lens(label, value.toInt))
      }
    } else {
      Remove(str.split("-").head)
    }
  }
  def toHash(str: String): Long = {
    str.toCharArray.foldLeft(0L)((acc, c) => (acc + c) * 17 % 256)
  }
}

sealed abstract class Op(val label: String)
case class Add(lens: Lens) extends Op(lens.label)
case class Remove(l: String) extends Op(l)

case class Lens(label: String, value: Int)
