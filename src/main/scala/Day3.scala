import scala.collection.mutable
import scala.io.Source

object Day3 {

  def main(args: Array[String]): Unit = {
    solveFor("#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2")
    solveFor(Source.fromResource("day3.txt").mkString)
  }

  def solveFor(input: String) = {
    val claims = input.split('\n').map(Claim.from)
    println(solve1For(claims))
    println(solve2For(claims))
  }

  def solve1For(claims: Array[Claim]): Int = {
    val buf = new mutable.HashMap[TwoD, Int]
    for {
      claim <- claims
      x <- claim.corner.x until (claim.corner.x + claim.size.x)
      y <- claim.corner.y until (claim.corner.y + claim.size.y)
      twoD <- Some(TwoD(x, y))
      _ <- buf.put(twoD, buf.getOrElse(twoD, 0) + 1)
    } yield ()
    buf.values.count(_ > 1)
  }

  def solve2For(claims: Array[Claim]): Int = {
    val buf = new mutable.HashMap[TwoD, Set[Int]]
    for {
      claim <- claims
      x <- claim.corner.x until (claim.corner.x + claim.size.x)
      y <- claim.corner.y until (claim.corner.y + claim.size.y)
      twoD <- Some(TwoD(x, y))
      _ <- buf.put(twoD, buf.getOrElse(twoD, Set()) + claim.id)
    } yield ()
    val values = buf.values.partition(_.size == 1)

    values._1
      .dropWhile(set1 => values._2.exists(set2 => set1.intersect(set2).nonEmpty))
      .head.head
  }

  case class Claim(id: Int, corner: TwoD, size: TwoD)

  object Claim {
    def from(line: String): Claim = {
      line.split(' ').toList match {
        case id :: _ :: corner :: size :: _ => Claim(id.drop(1).toInt, TwoD.from(corner.dropRight(1), ','), TwoD.from(size, 'x'))
      }
    }
  }

  case class TwoD(x: Int, y: Int)

  object TwoD {
    def from(input: String, separator: Char): TwoD = {
      input.split(separator).toList match {
        case x :: y :: _ => TwoD(x.toInt, y.toInt)
      }
    }
  }

}
