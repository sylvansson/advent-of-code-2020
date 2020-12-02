import scala.io.Source

object Day2 extends App {
  val pattern = "(\\d+)-(\\d+) ([a-z]): ([a-z]+)".r

  def entries = Source.fromResource("day2.txt").getLines.map {
    case pattern(int1, int2, char, password) =>
      (int1.toInt, int2.toInt, char.head, password)
  }

  def solvePart1: Int = entries.count { case (min, max, char, password) =>
    val count = password.count(_ == char)
    count >= min && count <= max
  }

  def solvePart2: Int = entries.count { case (pos1, pos2, char, password) =>
    password(pos1 - 1) == char ^ password(pos2 - 1) == char
  }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}