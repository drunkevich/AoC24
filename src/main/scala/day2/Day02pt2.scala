package advent
package day2

object Day02pt2 extends App {

  val sum = input.full.trim
    .split('\n')
    .map(Game.parse)
    .map(_.sets.fold(CubesSet(0, 0, 0)) {
      case (CubesSet(r1, g1, b1), CubesSet(r2, g2, b2)) =>
        CubesSet(r1 max r2, g1 max g2, b1 max b2)
    })
    .map(set => set.red * set.green * set.blue)
    .sum
  println(sum)

}
