import scala.io.Source

object Day5 {

  def main(args: Array[String]): Unit = {
    solveFor("aA")
    solveFor("abBA")
    solveFor("abAB")
    solveFor("aabAAB")
    solveFor("dabAcCaCBAcCcaDA")
    solveFor(Source.fromResource("day5.txt").mkString)
  }

  def solveFor(input: String) = {
    println(solve1For(input))
    println(solve2For(input))
  }

  def solve1For(input: String): Int = {
    input.foldRight(List[Char]())((c, acc) => acc match {
      case h :: t if shouldReact(h, c) => t
      case _ => c :: acc
    }).length
  }

  def solve2For(input: String): Int = {
    ('a' to 'z').map(c => solve1For(input.filter(_.toLower != c))).min
  }

  def shouldReact(unit1: Char, unit2: Char): Boolean = unit1.toLower == unit2.toLower && unit1 != unit2

}
