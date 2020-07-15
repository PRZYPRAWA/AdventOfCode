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

  def run(list: List[String])(mapFunc: String => Int): Int =
    Foldable[List].foldMap(list)(mapFunc)

  def mapInput1(n: Int): Int =
    n / 3 - 2

  def mapInput2(n: Int): List[Int] =
    n match {
      case _ if n <= 0 => Nil
      case _ => {
        val mapped = mapInput1(n)
        val mapped2 = if (mapped <= 0) 0 else mapped
        mapped2 :: mapInput2(mapped2)
      }
    }

}

object Day1Main extends App {
  import Day1._

  val lines = Day1.readFile
  println(Day1.run(lines)(e => mapInput1(e.toInt)))
  println(Day1.run(lines)(e => mapInput2(e.toInt).foldLeft(0)(_ |+| _)))
}
