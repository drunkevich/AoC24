package advent
package day11

case class Galaxy(x: Long, y: Long)

object Galaxy {
  def evalSumOfDistancesAfterExpansion(lines: Seq[String], expansionFactor: Long): Long = {
    val galaxies = {
      for {
        (line, y) <- lines.zipWithIndex
        (char, x) <- line.zipWithIndex
        if char == '#'
      } yield Galaxy(x, y)
    }

    val xExtensions = (0 until lines.head.length).filter(x => {
      lines.forall(line => line(x) == '.')
    })
    val yExtensions = lines.indices.filter(y => {
      lines(y).forall(_ == '.')
    })
    val extendedGalaxies = galaxies.map(g => {
      val dx = xExtensions.count(x => x < g.x) * (expansionFactor - 1)
      val dy = yExtensions.count(y => y < g.y) * (expansionFactor - 1)
      Galaxy(g.x + dx, g.y + dy)
    })

    val distances = for {
      i <- extendedGalaxies.indices
      g1 = extendedGalaxies(i)
      g2 <- extendedGalaxies.drop(i + 1)
    } yield Math.abs(g2.x - g1.x) + Math.abs(g2.y - g1.y)

    distances.sum
  }
}