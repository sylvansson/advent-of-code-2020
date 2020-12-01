import scala.io.Source

object Day1 extends App {
  val source = Source.fromResource("day1.txt")
  val entries = source.getLines.map(_.toInt).toList
  source.close

  def solve(n: Int): Long = entries
    .combinations(n)
    .collectFirst {
      case comb if comb.sum == 2020 => comb.product
    }
    .head

  println("Part 1: " + solve(2))
  println("Part 2: " + solve(3))
}