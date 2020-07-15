package aoc2018

import scala.annotation.tailrec
import scala.io.Source

object Day1 extends App {

  val input =
    Source.fromFile("src/main/scala/aoc2018/input-day1.txt").getLines.toList

  // Part 1
  def frequency(input: List[String]): Int =
    if (input.isEmpty) 0
    else input.head.toInt + frequency(input.tail)

  println(frequency(input))

  // Part 2
  def firstFreqTwice(input: List[String]): Int = {
    val inputUnchanged = input
    @tailrec
    def helper(input: List[String], frequencies: List[Int], freq: Int): Int = {
      if (frequencies.contains(freq)) freq
      else if (input.isEmpty)
        helper(
          inputUnchanged.tail,
          freq :: frequencies,
          freq + inputUnchanged.head.toInt
        )
      else helper(input.tail, frequencies :+ freq, freq + input.head.toInt)
    }
    helper(input.tail, List(0), input.head.toInt)
  }

  println(firstFreqTwice(input))
}
