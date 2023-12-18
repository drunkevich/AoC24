package advent
package day17

import util.{Position, W, *}

object Path {
  def eval(lines: Seq[String], min: Int, max: Int) = {
    val box: Position = Position(lines.head.length, lines.length)

    val heatMap = lines.map(_.toCharArray.map(_.toString.toInt)).toArray

    val pathNodes = scala.collection.mutable.Map.empty[Node, NodeInfo]
    pathNodes += Node(Position(0, 0), S) -> NodeInfo(0, Position(0, 0), S, true)
    pathNodes += Node(Position(0, 0), E) -> NodeInfo(0, Position(0, 0), E, true)

    def iterate(): Unit = {
      val nodesToCalculate = pathNodes.filter(_._2.calculate).toSeq
      if (nodesToCalculate.isEmpty) return
      calculate(nodesToCalculate)
      iterate()
    }

    def calculate(nodes: Seq[(Node, NodeInfo)]): Unit = {
      nodes.foreach { case (node, nodeInfo) =>
        val nextDirections = node.direction match {
          case N | S => Seq(E, W)
          case E | W => Seq(N, S)
        }
        for {
          direction <- nextDirections
          distance <- min to max
          nextPosition = node.position + (direction * distance)
          if nextPosition.in(box)
          heatLoss = (1 to distance).map { i =>
            node.position + (direction * i)
          }.map(p => heatMap(p.y)(p.x)).sum
          nextHeatLoss = nodeInfo.heatLoss + heatLoss
          if !pathNodes.get(Node(nextPosition, direction)).exists(_.heatLoss < nextHeatLoss)
        } do {
          if (pathNodes.contains(Node(nextPosition, direction)) && nextPosition == Position(box.x - 1, box.y - 1)) {
            println("xxx " + pathNodes(Node(nextPosition, direction)).heatLoss + " " + nextHeatLoss)
          }
          if (nextPosition == Position(box.x - 1, box.y - 1)) {
            println("found " + nextHeatLoss)
          }
          pathNodes += Node(nextPosition, direction) -> NodeInfo(nextHeatLoss, node.position, node.direction, true)
        }
        pathNodes += node -> nodeInfo.copy(calculate = false)
      }
    }

    iterate()


    val ends = Seq(N, S, E, W).flatMap { direction =>
      pathNodes.get(Node(Position(box.x - 1, box.y - 1), direction))
    }.sortBy(_.heatLoss)
    val end = ends.head


    val path = Iterator
      .iterate(end) { e =>
        pathNodes(Node(e.previousPosition, e.previousDirection))
      }
      .take(1000)
      .toSeq
      .reverse

    for (y <- 0 until box.y) {
      for (x <- 0 until box.x) {
        print(path.find(_.previousPosition == Position(x, y)).map(_.heatLoss).map(_ => "x").getOrElse("."))
      }
      println()
    }
    path.filter(_.heatLoss > 0).foreach(n => {
      print(n.heatLoss + " ")
    })
    println()

    end.heatLoss
  }
}

case class Node(position: Position, direction: Direction)
case class NodeInfo(heatLoss: Int, previousPosition: Position, previousDirection: Direction, calculate: Boolean)