package aoc2019

import scala.io.Source
import cats._
import cats.implicits._

object Day1 {
  def readFile: List[String] = {
    val filename = "src/main/scala/aoc2019/input-day1.txt"
    val source = Source.fromFile(filename)
    val lines = source.getLines().toList
    source.close()

    lines
  }

  def run(): Int =
    Foldable[List].foldMap(readFile)(e => mapInput(e.toInt))

  def mapInput(n: Int): Int =
    n / 3 - 2

}

object Day1Main extends App {
  println(Day1.run)
}
