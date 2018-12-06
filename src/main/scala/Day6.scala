import scala.collection.{immutable, mutable}
import scala.io.Source

object Day6 {

  def main(args: Array[String]): Unit = {
    solveFor("1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9", 32)
    solveFor(Source.fromResource("day6.txt").mkString, 10000)
  }

  def solveFor(input: String, threshold: Int) = {
    val coords = input
      .split('\n')
      .map(c => c.split(", ").map(_.toInt))
      .map(a => (a(0), a(1)))
    val xmin = coords.minBy(_._1)._1
    val ymin = coords.minBy(_._2)._2
    val xmax = coords.maxBy(_._1)._1
    val ymax = coords.maxBy(_._2)._2

    val allDist = new mutable.HashMap[(Int, Int), List[((Int, Int), Int)]]
    for {
      x <- xmin to xmax
      y <- ymin to ymax
      _ <- allDist.put(
        (x, y),
        coords.map(c => (c, distance((x, y), c))).sortBy(_._2).toList)
    } yield ()

    println(solve1For(coords, allDist, xmin, xmax, ymin, ymax))
    println(solve2For(allDist, threshold))
  }

  def solve1For(coords: Array[(Int, Int)],
                allDist: mutable.HashMap[(Int, Int), List[((Int, Int), Int)]],
                xmin: Int,
                xmax: Int,
                ymin: Int,
                ymax: Int): Int = {
    val m: collection.Map[(Int, Int), Int] = allDist.mapValues({
      case h1 :: h2 :: _ if h1._2 == h2._2 => -1
      case h :: _ => coords.indexOf(h._1)
    })

    val margins: immutable.IndexedSeq[Int] = (for {
      x <- xmin until xmax
    } yield m(x, ymin)) ++ (for {
      x <- xmin until xmax
    } yield m(x, ymax)) ++ (for {
      y <- ymin until ymax
    } yield m(xmin, y)) ++ (for {
      y <- ymin until ymax
    } yield m(xmax, y))

    (0 to coords.length)
      .diff(margins.toSet.toSeq)
      .map(i => m.values.count(_ == i))
      .max
  }

  def solve2For(allDist: mutable.HashMap[(Int, Int), List[((Int, Int), Int)]],
                threshold: Int): Int = {
    allDist.mapValues(_.map(_._2).sum).count(_._2 < threshold)
  }

  private def distance(p: (Int, Int), q: (Int, Int)): Int =
    Math.abs(p._1 - q._1) + Math.abs(p._2 - q._2)

}
