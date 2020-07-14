package aoc2018

import scala.collection.mutable
import scala.io.Source

case class Point(x: Int, y: Int)

object Day3 {
  def readFile: List[String] = {
    val filename = "src/main/scala/aoc2018/input-day3.txt"
    val source = Source.fromFile(filename)
    val lines = source.getLines().toList
    source.close()

    lines
  }


  def countMap(input: List[String]) = {
    input
      .flatMap(line => {
        val arr = line.split(" ")
        val point = arr(2).dropRight(1).split(",")
        val size = arr(3).split("x")

        mapToPoints((point(0).toInt, point(1).toInt), (size(0).toInt, size(1).toInt))
      })
      .foldLeft(mutable.Map.empty[Point, Int]) { (a, b) =>
        a += (b -> (a.getOrElse(b, 0) + 1))
      }
      .count(point => point._2 > 1)
  }


  def mapToPoints(startingPoint: (Int, Int), size: (Int, Int)): List[Point] = {
    var list: List[Point] = List()
    val startingX = startingPoint._1
    val startingY = startingPoint._2
    val sizeX = size._1
    val sizeY = size._2

    val X = startingX until startingX + sizeX
    val Y = startingY until startingY + sizeY
    for (x <- X; y <- Y)
      list = Point(x, y) :: list
    list
  }
}

object Day3Main extends App {
  val lines = Day3.readFile

  val map = Day3.countMap(lines)
  println(map)
}

