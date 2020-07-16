package aoc2015

import scala.util.matching.Regex
import scala.io.Source
import scala.collection.immutable.HashMap

object Day7 {

  def readFile: List[String] = {
    val filename = "src/main/scala/aoc2015/input-day7.txt"
    val source = Source.fromFile(filename)
    val lines = source.getLines().toList
    source.close()

    lines
  }

  val provide = raw"([a-z]+) -> (\w+)".r
  val provideNumber = raw"(\d+) -> (\w+)".r

  val notOp1 = raw"NOT ([a-z]+) -> (\w+)".r
  val notOp2 = raw"NOT (\d+) -> (\w+)".r

  val andOp1 = raw"([a-z]+) AND ([a-z]+) -> (\w+)".r
  val andOp2 = raw"([a-z]+) AND (\d+) -> (\w+)".r
  val andOp3 = raw"(\d+) AND ([a-z]+) -> (\w+)".r
  val andOp4 = raw"(\d+) AND (\d+) -> (\w+)".r

  val orOp1 = raw"([a-z]+) OR ([a-z]+) -> (\w+)".r
  val orOp2 = raw"([a-z]+) OR (\d+) -> (\w+)".r
  val orOp3 = raw"(\d+) OR ([a-z]+) -> (\w+)".r
  val orOp4 = raw"(\d+) OR (\d+) -> (\w+)".r

  val leftShift1 = raw"([a-z]+) LSHIFT ([a-z]+) -> (\w+)".r
  val leftShift2 = raw"([a-z]+) LSHIFT (\d+) -> (\w+)".r
  val leftShift3 = raw"(\d+) LSHIFT ([a-z]+) -> (\w+)".r
  val leftShift4 = raw"(\d+) LSHIFT (\d+) -> (\w+)".r

  val rightShift1 = raw"([a-z]+) RSHIFT ([a-z]+) -> (\w+)".r
  val rightShift2 = raw"([a-z]+) RSHIFT (\d+) -> (\w+)".r
  val rightShift3 = raw"(\d+) RSHIFT ([a-z]+) -> (\w+)".r
  val rightShift4 = raw"(\d+) RSHIFT (\d+) -> (\w+)".r

  def run(input: List[String]): HashMap[String, Int] = {
    def loop(instructions: List[String], hashMap: HashMap[String, Int]): HashMap[String, Int] = {
      //   println((hashMap, instructions.head))
      instructions match {
        case Nil => hashMap
        case head :: tl =>
          head match {
            case provideNumber(number, to) =>
              loop(tl, hashMap + (to -> number.toInt))

            case provide(from, to) =>
              if (hashMap.contains(from))
                loop(tl, hashMap + (to -> hashMap(from)))
              else
                loop(tl ::: List(head), hashMap)

            case notOp1(from, to) =>
              if (hashMap.contains(from))
                loop(tl, hashMap + (to -> (~hashMap(from))))
              else
                loop(tl ::: List(head), hashMap)

            case notOp2(number, to) =>
              loop(tl, hashMap + (to -> (~number.toInt)))

            case andOp1(first, second, to) =>
              if (hashMap.contains(first) && hashMap.contains(second))
                loop(tl, hashMap + (to -> (hashMap(first) & hashMap(second))))
              else
                loop(tl ::: List(head), hashMap)

            case andOp2(first, second, to) =>
              if (hashMap.contains(first))
                loop(tl, hashMap + (to -> (hashMap(first) & second.toInt)))
              else
                loop(tl ::: List(head), hashMap)

            case andOp3(first, second, to) =>
              if (hashMap.contains(second))
                loop(tl, hashMap + (to -> (hashMap(second) & first.toInt)))
              else
                loop(tl ::: List(head), hashMap)

            case andOp4(first, second, to) =>
              loop(tl, hashMap + (to -> (first.toInt & second.toInt)))

            case orOp1(first, second, to) =>
              if (hashMap.contains(first) && hashMap.contains(second))
                loop(tl, hashMap + (to -> (hashMap(first) | hashMap(second))))
              else
                loop(tl ::: List(head), hashMap)

            case orOp2(first, second, to) =>
              if (hashMap.contains(first))
                loop(tl, hashMap + (to -> (hashMap(first) | second.toInt)))
              else
                loop(tl ::: List(head), hashMap)

            case orOp3(first, second, to) =>
              if (hashMap.contains(second))
                loop(tl, hashMap + (to -> (hashMap(second) | first.toInt)))
              else
                loop(tl ::: List(head), hashMap)

            case orOp4(first, second, to) =>
              loop(tl, hashMap + (to -> (first.toInt | second.toInt)))

            case leftShift1(first, second, to) =>
              if (hashMap.contains(first) && hashMap.contains(second))
                loop(tl, hashMap + (to -> (hashMap(first) << hashMap(second))))
              else
                loop(tl ::: List(head), hashMap)

            case leftShift2(first, second, to) =>
              if (hashMap.contains(first))
                loop(tl, hashMap + (to -> (hashMap(first) << second.toInt)))
              else
                loop(tl ::: List(head), hashMap)

            case leftShift3(first, second, to) =>
              if (hashMap.contains(second))
                loop(tl, hashMap + (to -> (first.toInt << hashMap(second))))
              else
                loop(tl ::: List(head), hashMap)

            case leftShift4(first, second, to) =>
              loop(tl, hashMap + (to -> (first.toInt << second.toInt)))

            case rightShift1(first, second, to) =>
              if (hashMap.contains(first) && hashMap.contains(second))
                loop(tl, hashMap + (to -> (hashMap(first) >> hashMap(second))))
              else
                loop(tl ::: List(head), hashMap)

            case rightShift2(first, second, to) =>
              if (hashMap.contains(first))
                loop(tl, hashMap + (to -> (hashMap(first) >> second.toInt)))
              else
                loop(tl ::: List(head), hashMap)

            case rightShift3(first, second, to) =>
              if (hashMap.contains(second))
                loop(tl, hashMap + (to -> (first.toInt >> hashMap(second))))
              else
                loop(tl ::: List(head), hashMap)

            case rightShift4(first, second, to) =>
              loop(tl, hashMap + (to -> (first.toInt >> second.toInt)))
          }
      }
    }

    loop(input.sorted, HashMap[String, Int]())
  }

}

object Day7Main extends App {
  val input = Day7.readFile

  val result = Day7.run(input)

  println(result("a"))

  val input2 = (s"${result("a")} -> b") :: input.dropWhile(e => e.matches("(\\d+) -> b"))

  println(Day7.run(input2)("a"))
}
