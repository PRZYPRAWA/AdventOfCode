package aoc2018

import scala.io.Source

object Day2 extends App {
  val input = Source.fromFile("src/main/scala/aoc2018/input-day2.txt").getLines.toList

  def countLetter(text: String, howManyTimes: Int): Boolean = {
    def helper(text: String, n: Int, letterToCount: Char): Boolean = {
      if (text.isEmpty && n == 0) true
      else if (text.isEmpty) false
      else if (text.head == letterToCount) helper(text.tail, n - 1, letterToCount)
      else helper(text.tail, n, letterToCount)
    }
    for (i <- 0 to (text.length - 1)) {
      val letter = text.charAt(i)
      if (helper(text, howManyTimes, letter)) return true
    }
    false
  }

  def checksum(input: List[String], letter2times: Int, letter3times: Int): Int = {
    if (input.isEmpty) letter2times * letter3times
    else if (countLetter(input.head, 2) && countLetter(input.head, 3))
      checksum(input.tail, letter2times + 1, letter3times + 1)
    else if (countLetter(input.head, 2)) checksum(input.tail, letter2times + 1, letter3times)
    else if (countLetter(input.head, 3)) checksum(input.tail, letter2times, letter3times + 1)
    else checksum(input.tail, letter2times, letter3times)
  }

  println("Something")
  println(checksum(input, 0, 0))
}
