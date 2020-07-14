package aoc2019

import scala.io.Source

object Day2 {
  def readFile: List[String] = {
    val filename = "src/main/scala/aoc2019/input-day2.txt"
    val source = Source.fromFile(filename)
    val lines = source.getLines().toList
    source.close()

    lines.head.split(",").toList
  }

  def run(list: List[Int]): List[Int] = {
    def loop(nextId: Int, instructions: List[Int]): List[Int] =
      instructions.drop(nextId) match {
        case Nil     => instructions
        case 99 :: _ => instructions
        case 1 :: a :: b :: c :: _ => {
          val res = instructions(a) + instructions(b)
          loop(nextId + 4, instructions.updated(c, res))
        }
        case 2 :: a :: b :: c :: _ => {
          val res = instructions(a) * instructions(b)
          loop(nextId + 4, instructions.updated(c, res))
        }
      }
    loop(0, list)
  }
}

object Day2Main extends App {
  val ints = Day2.readFile.map(_.toInt)
  println(Day2.run(ints.updated(1, 12).updated(2, 2)))

  for (x <- 0 until 100; y <- 0 until 100) {
    val res = Day2.run(ints.updated(1, x).updated(2, y))
    if (res.head == 19690720) println((100 * x + y))
  }
}
