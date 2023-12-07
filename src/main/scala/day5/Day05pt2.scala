package advent
package day5

import util.Advent

object Day05pt2 extends Advent("day05_pt2") {
  override def eval(lines: Seq[String]) = {
    val seedRanges = Parser
      .seeds(lines.head)
      .grouped(2)
      .map {
        case Seq(a, b) => Range(a, a + b - 1)
      }
      .toArray

    val seedToSoil = Parser.mapping("seed-to-soil map:", lines)
    val soilToFertilizer = Parser.mapping("soil-to-fertilizer map:", lines)
    val fertilizerToWater = Parser.mapping("fertilizer-to-water map:", lines)
    val waterToLight = Parser.mapping("water-to-light map:", lines)
    val lightToTemperature = Parser.mapping("light-to-temperature map:", lines)
    val temperatureToHumidity =
      Parser.mapping("temperature-to-humidity map:", lines)
    val humidityToLocation = Parser.mapping("humidity-to-location map:", lines)

    seedRanges
      .flatMap(seedToSoil.mapRange)
      .flatMap(soilToFertilizer.mapRange)
      .flatMap(fertilizerToWater.mapRange)
      .flatMap(waterToLight.mapRange)
      .flatMap(lightToTemperature.mapRange)
      .flatMap(temperatureToHumidity.mapRange)
      .flatMap(humidityToLocation.mapRange)
      .map(_.start)
      .min
      .toString
  }
}
