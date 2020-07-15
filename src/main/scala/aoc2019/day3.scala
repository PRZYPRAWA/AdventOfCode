package aoc2019

import scala.io.Source
import cats._
import cats.implicits._

object Day3 {
  case class Point(x: Int, y: Int)

  def combineAll[F[_]: Foldable, A: Monoid](as: F[A]): A =
    Foldable[F].foldLeft(as, Monoid.empty[A])(_ |+| _)

  def readFile: List[String] = {
    val filename = "src/main/scala/aoc2019/input-day3.txt"
    val source = Source.fromFile(filename)
    val lines = source.getLines().toList
    source.close()

    lines
  }

  type Direction = String
  type Len = Int
  type Quantity = Int

  def stringToInstructions(list: List[String]): List[(Direction, Len)] =
    list.map { s =>
      val (dir, len) = s.splitAt(1)
      val lenInt = len.toInt
      (dir, lenInt)
    }

  def run(lines: List[(Direction, Len)]): Map[Point, Quantity] = {
    def loop(
      point: Point,
      instructions: List[(Direction, Len)],
      points: List[Point]
    ): List[Point] = {
      instructions match {
        case Nil => points
        case ("R", value) :: t => {
          val length = (point.x to (point.x + value)).toList
          val newPoints = length.map(p => Point(p, point.y))
          loop(newPoints.last, t, newPoints ::: points)
        }
        case ("L", value) :: t => {
          val length = ((point.x - value) to point.x).toList
          val newPoints = length.map(p => Point(p, point.y))
          loop(newPoints.head, t, newPoints ::: points)
        }
        case ("U", value) :: t => {
          val length = (point.y to (point.y + value)).toList
          val newPoints = length.map(p => Point(point.x, p))
          loop(newPoints.last, t, newPoints ::: points)
        }
        case ("D", value) :: t => {
          val length = ((point.x - value) to point.x).toList
          val newPoints = length.map(p => Point(point.x, p))
          loop(newPoints.head, t, newPoints ::: points)
        }
      }
    }

    val startingPoint = Point(0, 0)
    loop(startingPoint, lines, List()).map(p => (p, 1)).toMap
  }
}

object Day3Main extends App {
  val lines = Day3.readFile.map(e => e.split(",").toList).toList

  val instructions1 = Day3.stringToInstructions(lines(0))
  val res1 = Day3.run(instructions1)

  val instructions2 = Day3.stringToInstructions(lines(1))
  val res2 = Day3.run(instructions2)

  import scala.math.abs
  println(
    Day3
      .combineAll(List(res1, res2))
      .filter(p => p._2 >= 2)
      .toList
      .filter { p =>
        {
          val point = p._1
          point.x != 0 && point.y != 0
        }
      }
      .minBy { p =>
        {
          val point = p._1
          abs(point.x) + abs(point.y)
        }
      }
  )
}
