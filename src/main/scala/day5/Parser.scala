package advent
package day5


object Parser {
  val seedsPattern = """^seeds:(.*)$""".r
  def seeds(string: String): Seq[Long] = {
    string match {
      case seedsPattern(ids) =>
        ids.split(" ").filter(_.nonEmpty).map(_.toLong)
    }
  }

  def mapping(name: String, lines: Seq[String]): Mappings = {
    Mappings(lines.dropWhile(_ != name).drop(1).takeWhile(!_.isBlank).map { line =>
      val longs = line.split(" ").filter(_.nonEmpty).map(_.toLong)
      Mapping(longs(0), longs(1), longs(2))
    })
  }
}

case class Range(start: Long, end: Long)
case class MapResult(unmapped: Seq[Range], mapped: Seq[Range])

trait Position
case object Left extends Position
case object Right extends Position
case object Middle extends Position

case class Mapping(destination: Long, source: Long, length: Long) {
  val map: PartialFunction[Long, Long] = {
    case value if value >= source && value <= source + length - 1=>
      destination - source + value
  }

  def mapRange(range: Range): MapResult = {
    split(range) match {
      case MapResult(unmapped, mapped) => MapResult(unmapped, mapped.map {
        case Range(start, end) => Range(map(start), map(end))
      })
    }
  }

  private def position(long: Long): Position = {
    if (long < source) Left
    else if (long > source + length - 1) Right
    else Middle
  }

  private def split(range: Range): MapResult = range match {
    case Range(start, end) if position(start) == Left && position(end) == Left => MapResult(Seq(range), Nil)
    case Range(start, end) if position(start) == Right && position(end) == Right => MapResult(Seq(range), Nil)
    case Range(start, end) if position(start) == Middle && position(end) == Middle => MapResult(Nil, Seq(range))
    case Range(start, end) if position(start) == Left && position(end) == Middle => MapResult(Seq(Range(start, source - 1)), Seq(Range(source, end)))
    case Range(start, end) if position(start) == Middle && position(end) == Right => MapResult(Seq(Range(source + length, end)), Seq(Range(start, source + length - 1)))
    case Range(start, end) if position(start) == Left && position(end) == Right => MapResult(Seq(Range(start, source - 1), Range(source + length, end)), Seq(Range(source, source + length - 1)))
  }
}
case class Mappings(mappings: Seq[Mapping]) {
  def map(value: Long): Long = {
    mappings.find(_.map.isDefinedAt(value)).map(_.map(value)).getOrElse(value)
  }

  def mapRange(range: Range): Seq[Range] = {
    val finalMapping = mappings.foldLeft(MapResult(Seq(range), Nil)) {
      case (MapResult(unmapped, mapped), mapping) =>
        val newResult = unmapped.map(mapping.mapRange)
        val newUnmapped = newResult.flatMap(_.unmapped)
        val newMapped = newResult.flatMap(_.mapped)
        MapResult(newUnmapped, newMapped ++ mapped)
    }
    finalMapping.unmapped ++ finalMapping.mapped
  }

}
