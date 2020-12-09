import scala.io.Source

object Day9 extends App {
  def entries = Source.fromResource("day9.txt").getLines.map(_.toLong)

  val preamble = 25

  def solvePart1: Long = {
    (for {
      window <- entries.sliding(preamble + 1)
      (nums, Seq(sum)) = window.splitAt(preamble)
      if !nums.combinations(2).exists(_.sum == sum)
    } yield sum).next
  }

  def solvePart2: Long = {
    val sum = solvePart1
    (for {
      n <- (2 to entries.size).view
      window <- entries.sliding(n) if window.sum == sum
    } yield window.min + window.max).head
  }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}