import scala.annotation.tailrec
import scala.io.Source

object Day13 extends App {
  def input = Source.fromResource("day13.txt").getLines.toList match {
    case List(timestamp, buses) => (
      timestamp.toInt,
      buses.split(",").map(_.toIntOption).toList
    )
  }

  val (earliest, buses) = input

  def solvePart1: Int = {
    val (bus, departure) = buses
      .flatten
      .map(id => (id, earliest - earliest % id + id))
      .minBy(_._2)
    bus * (departure - earliest)
  }

  def solvePart2: Long = {
    def check(timestamp: Long, n: Int) =
      buses.take(n).zipWithIndex.forall {
        case (Some(id), index) => (timestamp + index) % id == 0
        case _ => true
      }

    @tailrec
    def loop(n: Int, timestamp: Long): Long = {
      val prod = buses.take(n).flatten.map(_.toLong).product
      // Check whether the n + 1 first buses depart
      // one minute apart.
      if (check(timestamp, n + 1)) {
        // We're at the last bus, so the current
        // timestamp is our answer.
        if (n == buses.length)
          timestamp
        else loop(n + 1, timestamp)
      } else {
        loop(n, timestamp + prod)
      }
    }

    loop(n = 1, timestamp = 0)
  }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}