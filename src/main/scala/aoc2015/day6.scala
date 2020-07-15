package aoc2015

import scala.util.matching.Regex
import scala.io.Source
import scala.collection.mutable.ArrayBuffer // used because of the efficiency problem 

object Day6 {
  def readFile: List[String] = {
    val filename = "src/main/scala/aoc2015/input-day6.txt"
    val source = Source.fromFile(filename)
    val lines = source.getLines().toList
    source.close()

    lines
  }

  def run(list: List[String]): ArrayBuffer[ArrayBuffer[Int]] = {
    val instruction =
      raw"(toggle|turn on|turn off) (\d+,\d+) through (\d+,\d+)".r

    def loop(
        instructions: List[String],
        grid: ArrayBuffer[ArrayBuffer[Int]]
    ): ArrayBuffer[ArrayBuffer[Int]] =
      if (instructions == Nil) {
        grid
      } else {
        val first = instruction.findAllIn(instructions.head).group(1)

        val second = takePoints(
          instruction.findAllIn(instructions.head).group(2)
        )

        val third = takePoints(
          instruction.findAllIn(instructions.head).group(3)
        )

        if (first == "toggle") {
          loop(
            instructions.tail,
            toggle(grid, second, third, e => if (e == 0) 1 else 0)
          )
        } else if (first == "turn on") {
          loop(instructions.tail, turnOn(grid, second, third))
        } else {
          loop(instructions.tail, turnOff(grid, second, third))
        }
      }

    loop(list, ArrayBuffer.fill(1000)(ArrayBuffer.fill(1000)(0)))
  }

  def takePoints(str: String): (Int, Int) = {
    val Array(first, second) = str.split(",")
    (first.toInt, second.toInt)
  }

  def toggle(
      grid: ArrayBuffer[ArrayBuffer[Int]],
      first: (Int, Int),
      second: (Int, Int),
      func: Int => Int
  ): ArrayBuffer[ArrayBuffer[Int]] = {
    val points = (for {
      x <- first._1 to second._1
      y <- first._2 to second._2
    } yield (x, y)).toList

    points.foldLeft(grid) { (acc, point) =>
      acc(point._1)(point._2) = func(acc(point._1)(point._2))
      acc
    }
  }

  def turnOn(
      grid: ArrayBuffer[ArrayBuffer[Int]],
      first: (Int, Int),
      second: (Int, Int)
  ): ArrayBuffer[ArrayBuffer[Int]] = toggle(grid, first, second, e => 1)

  def turnOff(
      grid: ArrayBuffer[ArrayBuffer[Int]],
      first: (Int, Int),
      second: (Int, Int)
  ): ArrayBuffer[ArrayBuffer[Int]] = toggle(grid, first, second, e => 0)

}

object Day6Main extends App {
  val lines = Day6.readFile

  println(lines.head)
  println(Day6.run(lines).flatten.foldLeft(0)(_ + _))
}
