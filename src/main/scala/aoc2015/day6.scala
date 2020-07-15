package aoc2015

import scala.util.matching.Regex
import scala.io.Source

object Day6 {
  def readFile: List[String] = {
    val filename = "src/main/scala/aoc2015/input-day6.txt"
    val source = Source.fromFile(filename)
    val lines = source.getLines().toList
    source.close()

    lines
  }

  def run(list: List[String]): List[List[Int]] = {
    val instruction =
      raw"(toggle|turn on|turn off) (\d+,\d+) through (\d+,\d+)".r

    def loop(
        instructions: List[String],
        grid: List[List[Int]]
    ): List[List[Int]] =
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

    loop(list, List.fill(1000)(List.fill(1000)(0)))
  }

  def takePoints(str: String): (Int, Int) = {
    val Array(first, second) = str.split(",")
    (first.toInt, second.toInt)
  }

  def toggle(
      grid: List[List[Int]],
      first: (Int, Int),
      second: (Int, Int),
      func: Int => Int
  ): List[List[Int]] = {
    val points = (for {
      x <- first._1 to second._1
      y <- first._2 to second._2
    } yield (x, y)).toList

    points.foldLeft(grid)((acc, point) =>
      acc.updated(
        point._1,
        acc(point._1)
          .updated(point._2, func(acc(point._1)(point._2)))
      )
    )
  }

  def turnOn(
      grid: List[List[Int]],
      first: (Int, Int),
      second: (Int, Int)
  ): List[List[Int]] = toggle(grid, first, second, e => 1)

  def turnOff(
      grid: List[List[Int]],
      first: (Int, Int),
      second: (Int, Int)
  ): List[List[Int]] = toggle(grid, first, second, e => 0)

}

object Day6Main extends App {
  val lines = Day6.readFile

  println(lines.head)
  println(Day6.run(lines).head.take(100))
}
