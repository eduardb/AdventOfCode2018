import java.text.SimpleDateFormat
import java.util.Date

import scala.io.Source

object Day4 {

  def main(args: Array[String]): Unit = {
    solveFor(Source.fromResource("day4ex.txt").mkString)
    solveFor(Source.fromResource("day4.txt").mkString)
  }

  def solveFor(input: String) = {
    val records = input.split('\n')
      .sorted
      .map(Record.from)

    def sleepTimes = {
      var guardId: Int = 0
      var startTime: Date = null
      for {
        record <- records
        sleepTime <- record match {
          case Record(_, BeginsShift(id)) => guardId = id; None
          case Record(date, FallsAsleep) => startTime = date; None
          case Record(date, WakesUp) => Some(guardId, startTime.getMinutes until date.getMinutes)
        }
      } yield sleepTime
    }

    val guardsToSleepTimes = sleepTimes.groupBy(_._1)
      .mapValues(a => a.map(_._2))

    println(solve1For(guardsToSleepTimes))
    println(solve2For(guardsToSleepTimes))
  }

  def solve1For(guardsToSleepTimes: Map[Int, Array[Range]]): (Int, Int, Int)= {
    val mostSleepyId = guardsToSleepTimes
      .mapValues(l => l.map(_.length).sum)
      .maxBy(_._2)
      ._1

    val mostSleepyTimes = guardsToSleepTimes(mostSleepyId)
    val mostSleepyMinute = findMostSleepyMinute(mostSleepyTimes)._1

    (mostSleepyId, mostSleepyMinute, mostSleepyId * mostSleepyMinute)
  }

  private def findMostSleepyMinute(ranges: Array[Range]): (Int, Int) = {
    (0 until 60).map(minute =>
      (minute, ranges.count(range => range contains minute))
    ).maxBy(_._2)
  }

  def solve2For(guardsToSleepTimes: Map[Int, Array[Range]]): (Int, Int, Int) = {
    val mostSleepy = guardsToSleepTimes.mapValues(findMostSleepyMinute)
      .maxBy(_._2._2)

    (mostSleepy._1, mostSleepy._2._1, mostSleepy._1 * mostSleepy._2._1)
  }

  sealed trait RecordType
  case class BeginsShift(id: Int) extends RecordType
  case object FallsAsleep extends RecordType
  case object WakesUp extends RecordType

  case class Record(date: Date, recordType: RecordType)

  object Record {

    private val BEGINS_SHIFT = "\\[(.*)\\] Guard #(\\d+) begins shift".r
    private val FALLS_ASLEEP = "\\[(.*)\\] falls asleep".r
    private val WAKES_UP     = "\\[(.*)\\] wakes up".r

    private val format = new SimpleDateFormat("yyyy-MM-dd hh:mm")

    def from(raw: String): Record = {
      raw match {
        case BEGINS_SHIFT(date, id) => Record(format.parse(date), BeginsShift(id.toInt))
        case FALLS_ASLEEP(date)     => Record(format.parse(date), FallsAsleep)
        case WAKES_UP(date)         => Record(format.parse(date), WakesUp)
      }
    }
  }
}
