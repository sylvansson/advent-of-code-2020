import scala.annotation.tailrec
import scala.io.Source

object Day5 extends App {
  def entries = Source.fromResource("day5.txt").getLines

  case class SeatPosition(row: Int, column: Int) {
    val id: Int = row * 8 + column
  }

  def mid(range: Range) = range.start + (range.end - range.start) / 2

  @tailrec
  def seatPosition(instructions: List[Char], rows: Range = 0 to 127, columns: Range = 0 to 7): SeatPosition = {
    if (instructions.isEmpty) SeatPosition(rows.end, columns.end)
    else {
      val curr :: rest = instructions
      curr match {
        case 'F' => seatPosition(rest, rows.start to mid(rows), columns)
        case 'B' => seatPosition(rest, mid(rows) + 1 to rows.end, columns)
        case 'L' => seatPosition(rest, rows, columns.start to mid(columns))
        case 'R' => seatPosition(rest, rows, mid(columns) + 1 to columns.end)
      }
    }
  }

  def sortedSeatIds = entries
    .map(_.toCharArray.toList)
    .map(inst => seatPosition(inst))
    .map(_.id)
    .toList
    .sorted

  def solvePart1 = sortedSeatIds.last

  def solvePart2 = sortedSeatIds
    .sliding(2)
    .collectFirst {
      case List(a, b) if a + 2 == b => a + 1
    }
    .head

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}