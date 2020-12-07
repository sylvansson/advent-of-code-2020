import scala.io.Source

object Day6 extends App {
  def entries = Source.fromResource("day6.txt").getLines

  def solve(op: (Set[Char], Set[Char]) => Set[Char]): Int = {
    val (Some(answers), sum) = entries.foldLeft((None: Option[Set[Char]], 0)) {
      case ((None, sum), curr) => (Some(curr.toSet), sum)
      case ((Some(answers), sum), curr) =>
        // The line is empty, so a group just ended.
        if (curr.isEmpty)
          (None, sum + answers.size)
        else
          (Some(op(answers, curr.toSet)), sum)
    }
    sum + answers.size
  }

  println("Part 1: " + solve(_ union _))
  println("Part 2: " + solve(_ intersect _))
}