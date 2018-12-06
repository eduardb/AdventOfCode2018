import scala.collection.mutable
import scala.io.Source

object Day1 {

  def main(args: Array[String]): Unit = {
    println(solveFor("+1\n-1"))
    println(solveFor("+3\n+3\n+4\n-2\n-4"))
    println(solveFor("-6\n+3\n+8\n+5\n-6"))
    println(solveFor("+7\n+7\n-2\n-7\n-4"))
    println(solveFor(Source.fromResource("day1.txt").mkString))
  }

  def solveFor(input: String): (Int, Int) = {
    val process = input.split('\n').map(_.toInt)
    (solve1For(process), solve2For(process))
  }

  def solve1For(input: Array[Int]): Int = input.sum

  def solve2For(input: Array[Int]): Int = {
    val buf = new mutable.HashMap[Int, Int]
    Stream.continually(input.toStream).flatten
      .scanLeft(0)(_ + _)
      .takeWhile { freq =>
        buf(freq) = buf.getOrElse(freq, 0) + 1
        if (buf(freq) == 2) return freq
        true
      }.last
  }

}