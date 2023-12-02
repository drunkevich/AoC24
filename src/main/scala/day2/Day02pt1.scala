package advent
package day2


object Day02pt1 extends App {

  val red = 12
  val green = 13
  val blue = 14

  val sum = input.full.trim
    .split('\n')
    .map(Game.parse)
    .filter(_.sets.forall(turn => {
        turn.red <= red && turn.blue <= blue && turn.green <= green
      })
    )
    .map(_.id)
    .sum
  println(sum)

}
