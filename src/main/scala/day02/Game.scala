package advent
package day02

case class Game(id: Int, sets: Seq[CubesSet])
case class CubesSet(red: Int, blue: Int, green: Int)
object Game {
  val gamePattern = """^Game (\d+):(.*)$""".r
  def parse(string: String): Game = {
    string match {
      case gamePattern(id, sets) => Game(id.toInt, CubesSet.parseSets(sets))
    }
  }
}
object CubesSet {
  val setPattern = """^(\d+) (\w+)$""".r

  def parseSets(string: String): Seq[CubesSet] = {
    string.trim.split(';').map(parse)
  }
  def parse(string: String): CubesSet = {
    string.trim.split(',').map(_.trim).foldLeft(CubesSet(0,0,0))((set, cubeSet) => {
      cubeSet match {
        case setPattern(count, color) => {
          color match {
            case "red" => set.copy(red = count.toInt)
            case "blue" => set.copy(blue = count.toInt)
            case "green" => set.copy(green = count.toInt)
          }
        }
      }
    })
  }
}