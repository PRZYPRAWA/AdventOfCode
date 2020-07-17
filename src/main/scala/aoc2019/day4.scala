package aoc2019

object Day4 {

  def isPossiblePassword(input: Int): Boolean = {
    val inputString = input.toString
    if (isSorted(inputString.toList) && containsDouble(inputString.toList))
      true
    else
      false
  }

  def isSorted[A](list: List[A])(implicit ordering: Ordering[A]): Boolean = list == list.sorted(ordering)

  def containsDouble[A](input: List[A]): Boolean =
    input match {
      case Nil          => false
      case _ :: Nil     => false
      case h :: s :: tl => if (h == s) true else containsDouble(s :: tl)
    }

}

object Day4Main extends App {
  println((240298 to 784956).filter(Day4.isPossiblePassword(_)).length)
}
