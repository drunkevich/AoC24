package advent
package day05

import util.Advent

object Day05pt1 extends Advent("day05_pt1") {
  override def eval(lines: Seq[String]) = {
    val seeds = Parser.seeds(lines.head)

    val seedToSoil = Parser.mapping("seed-to-soil map:", lines)
    val soilToFertilizer = Parser.mapping("soil-to-fertilizer map:", lines)
    val fertilizerToWater = Parser.mapping("fertilizer-to-water map:", lines)
    val waterToLight = Parser.mapping("water-to-light map:", lines)
    val lightToTemperature = Parser.mapping("light-to-temperature map:", lines)
    val temperatureToHumidity =
      Parser.mapping("temperature-to-humidity map:", lines)
    val humidityToLocation = Parser.mapping("humidity-to-location map:", lines)

    seeds
      .map(seedToSoil.map)
      .map(soilToFertilizer.map)
      .map(fertilizerToWater.map)
      .map(waterToLight.map)
      .map(lightToTemperature.map)
      .map(temperatureToHumidity.map)
      .map(humidityToLocation.map)
      .min
  }
}
