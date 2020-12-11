import scala.collection.mutable
import scala.io.Source

object Day10 extends App {
  def entries = Source.fromResource("day10.txt").getLines.map(_.toInt)
  def adapters = {
    val sortedEntries = entries.toList.sorted
    0 +: sortedEntries :+ (sortedEntries.max + 3)
  }

  def solvePart1: Int = {
    val diffs = adapters.sliding(2).foldLeft(Map.empty[Int, Int]) {
      case (acc, List(a, b)) =>
        val diff = b - a
        acc + (diff -> (acc.getOrElse(diff, 0) + 1))
    }
    diffs(1) * diffs(3)
  }

  def solvePart2: Long = {
    val map = new mutable.HashMap[Int, Long]()
    def memoize(key: Int)(fn: => Long): Long = {
      map(key) = map.getOrElse(key, fn)
      map(key)
    }

    def numArrangements(adapters: List[Int]): Long = memoize(adapters.head) {
      adapters match {
        case _ :: Nil => 1
        case x :: xs => xs
          .takeWhile(_ <= x + 3)
          .indices
          .map(i => numArrangements(adapters.drop(i + 1)))
          .sum
      }
    }
    numArrangements(adapters)
  }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}