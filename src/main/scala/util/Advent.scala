package advent
package util

import java.nio.file.{Files, FileSystems}
import scala.io.Source
import scala.jdk.CollectionConverters.*

abstract class Advent(dir: String) extends App {
  def evalSum(line: String): Long = ???
  def eval(lines: Seq[String]): Any = lines.map(evalSum).sum.toString

  def expected(fileName: String): Option[String] =
    if fileName.contains("_")
    then Some(fileName.split("_")(1))
    else None

  println()
  println("day     : " + dir)
  Files
    .list(FileSystems.getDefault.getPath("src/main/resources/" + dir))
    .iterator()
    .asScala
    .filter(Files.isRegularFile(_))
    .foreach(path => {
      val source = Source.fromFile(path.toFile)
      val lines = source.getLines().toSeq
      source.close()
      val result = eval(lines).toString

      println("file    : " + path.getFileName)
      println("result  : " + result)
      expected(path.getFileName.toString).foreach(expected => {
        println("expected: " + expected)
        assert(result == expected)
      })
    })

}
