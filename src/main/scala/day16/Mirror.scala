package advent
package day16

import util.*

import scala.annotation.{tailrec, targetName}

sealed trait Mirror
object Mirror {

  @targetName("pipe")
  case object | extends Mirror

  @targetName("slash")
  case object / extends Mirror

  @targetName("backslash")
  case object \ extends Mirror

  @targetName("dash")
  case object - extends Mirror

  def apply(c: Char): Mirror = c match {
    case '|' => |
    case '/' => /
    case '\\' => \
    case '-' => -
  }

  def reflect(mirror: Mirror, dir: Direction): Set[Direction] = mirror match {
    case Mirror./ => dir match {
      case N => Set(E)
      case E => Set(N)
      case S => Set(W)
      case W => Set(S)
    }
    case Mirror.\ => dir match {
      case N => Set(W)
      case E => Set(S)
      case S => Set(E)
      case W => Set(N)
    }
    case Mirror.- => dir match {
      case N | S => Set(E, W)
      case d => Set(d)
    }
    case Mirror.| => dir match {
      case E | W => Set(N, S)
      case d => Set(d)
    }
  }

  @tailrec
  def lightStep(oldLights: Set[(Position, Direction)], newLights: Set[(Position, Direction)], mirrors: Map[Position, Mirror], box: Position): Set[(Position, Direction)] = {
    val newLights2 = for {
      (pos, dir) <- newLights
      newPos = pos.plus(dir)
      if newPos.in(box)
      newDir <- mirrors.get(newPos).map(m => Mirror.reflect(m, dir)).getOrElse(Set(dir))
      if !oldLights.contains((newPos, newDir))
    } yield (newPos, newDir)
    if (newLights2.isEmpty) oldLights else lightStep(oldLights ++ newLights2, newLights2, mirrors, box)
  }
}