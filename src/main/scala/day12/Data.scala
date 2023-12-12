package advent
package day12

trait State
case object Operational extends State
case object Damaged extends State
case object Unknown extends State

case class Data(springs: List[State], controlSum: List[Int])

object Data{
  def parse(line: String): Data = {
    val springs = line.split(" ").head.map {
      case '.' => Operational
      case '#' => Damaged
      case '?' => Unknown
    }.toList
    val controlSum = line.split(" ").last.split(",").map(_.toInt).toList
    Data(springs, controlSum)
  }
}

object Solution {

  val cache = scala.collection.mutable.Map.empty[Data, Long]

  def apply(initial: Data): Long = {

    def generate(data: Data): Long = {
      val cacheValue = cache.get(data)
      if (cacheValue.isDefined) return cacheValue.get
      val result: Long = data.springs.headOption match {
        case None if data.controlSum.isEmpty => 1L
        case None if data.controlSum.nonEmpty => 0L
        case _ if data.controlSum.isEmpty => if (!data.springs.contains(Damaged)) 1L else 0L
        case _ if data.springs.length < data.controlSum.sum + data.controlSum.size - 1 => 0L
        case Some(Damaged) if data.controlSum.size == 1 =>
          val damagedGroupExists = !data.springs.take(data.controlSum.head).contains(Operational)
          val noDamagedAfterGroup = !data.springs.drop(data.controlSum.head).contains(Damaged)
          if (damagedGroupExists && noDamagedAfterGroup) 1L else 0L
        case Some(Operational) =>
          generate(Data(data.springs.tail, data.controlSum))
        case Some(Damaged) =>
          val damagedGroupExists = !data.springs.take(data.controlSum.head).contains(Operational)
          val damagedGroupEnds = data.springs(data.controlSum.head) != Damaged
          if (damagedGroupExists && damagedGroupEnds) generate(Data(data.springs.drop(data.controlSum.head + 1), data.controlSum.tail)) else 0L
        case Some(Unknown) =>
          val first = generate(Data(Damaged +: data.springs.tail, data.controlSum))
          val second = generate(Data(Operational +: data.springs.tail, data.controlSum))
          first + second
      }
      cache.put(data, result)
      result
    }

    val generated = generate(initial)

    generated
  }
}