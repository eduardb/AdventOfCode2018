import scala.io.Source

object Day2 {
  def main(args: Array[String]): Unit = {
    solveFor("abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab")
    solveFor("abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz")
    solveFor(Source.fromResource("day2.txt").mkString)
  }

  def solveFor(input: String) = {
    val split = input.split('\n')
    println(solve1For(split))
    println(solve2For(split))
  }

  def solve1For(input: Array[String]): Int = input.flatMap { box =>
    box.groupBy(k => k)
      .map(_._2.length)
      .filter(2 to 3 contains _)
      .toSeq.distinct
  }
    .groupBy(k => k)
    .map(e => e._2.length)
    .product

  def difference: ((String, String)) => Int = { t =>
    val (a, b) = t
    a.zip(b).foldLeft(0)((acc, pair) => if (pair == pair.swap) acc else acc + 1)
  }

  def solve2For(input: Array[String]): Any = {
    val tuples: Array[(String, String)] = for (x <- input; y <- input) yield (x, y)
    val (a, b) = tuples.filter(p => p.swap != p)
      .minBy(difference)
    a.intersect(b)
  }
}
