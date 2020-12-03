import scala.io.Source

object Day3 extends App {
  def entries = Source.fromResource("day3.txt").getLines

  def solve(right: Int, down: Int): Long = {
    val map = entries.toList
    val ys = map.indices by down
    val xs = LazyList.from(0).map(_ * right)
    (xs zip ys).count {
      case (x, y) => map(y)(x % map.head.length) == '#'
    }
  }

  val slopes = List(
    (1, 1),
    (3, 1),
    (5, 1),
    (7, 1),
    (1, 2),
  )
  val answerPart2 = slopes
    .map { case (right, down) => solve(right, down) }
    .product

  println("Part 1: " + solve(3, 1))
  println("Part 2: " + answerPart2)
}