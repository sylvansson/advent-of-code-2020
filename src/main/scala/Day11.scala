import scala.annotation.tailrec
import scala.io.Source

object Day11 extends App {
  def entries = Source.fromResource("day11.txt").getLines

  val offsets = List(0, 1, -1)

  case class SeatLayout(grid: List[List[Char]]) {
    def update(adjacentFn: ((Int, Int)) => List[(Int, Int)], threshold: Int): SeatLayout = {
      val ys = grid.indices.toList
      val xs = grid.head.indices.toList
      SeatLayout(ys.map(y =>
        xs.map(x => grid(y)(x) match {
          case 'L' =>
            if (!adjacentFn(x, y).exists(isOccupied)) '#' else 'L'
          case '#' =>
            if (adjacentFn(x, y).count(isOccupied) >= threshold) 'L' else '#'
          case '.' => '.'
        })
      ))
    }

    def isOccupied(coords: (Int, Int)): Boolean = {
      val (x, y) = coords
      grid(y)(x) == '#'
    }

    /** Get every immediately adjacent seat. */
    def adjacent(coords: (Int, Int)): List[(Int, Int)] = {
      val (x, y) = coords
      for {
        xOffset <- offsets
        yOffset <- offsets
        adj = (x + xOffset, y + yOffset) if adj != coords && isWithinBoundaries(adj)
      } yield adj
    }

    /**
     * Get the first visible seat in a given direction, if any.
     *
     * @param coords The starting position.
     * @param dir The direction, defined by position offsets.
     * @return The seat's coordinates, if any.
     */
    def firstSeatInDirection(coords: (Int, Int), dir: (Int, Int)): Option[(Int, Int)] = {
      val (x, y) = coords
      val (xOffset, yOffset) = dir
      LazyList.from(1)
        .map(i => (x + xOffset * i, y + yOffset * i))
        // Don't look any further if the point is outside the grid.
        .takeWhile(isWithinBoundaries)
        .find {
          case (x, y) => grid(y)(x) != '.'
        }
    }

    /** Get the seats that are visible from a given seat. */
    def withinLineOfSight(coords: (Int, Int)): List[(Int, Int)] = {
      for {
        xOffset <- offsets
        yOffset <- offsets if (xOffset, yOffset) != (0, 0)
        point <- firstSeatInDirection(coords, (xOffset, yOffset))
      } yield point
    }

    def isWithinBoundaries(coords: (Int, Int)): Boolean = {
      val (x, y) = coords
      (x >= 0 && x < grid.head.length) && y >= 0 && y < grid.length
    }
  }

  @tailrec
  def solve(
    layout: SeatLayout,
    adjacentFn: SeatLayout => ((Int, Int)) => List[(Int, Int)],
    threshold: Int
  ): Int = {
    val next = layout.update(adjacentFn(layout), threshold)
    if (next == layout)
      layout.grid.map(_.count(_ == '#')).sum
    else solve(next, adjacentFn, threshold)
  }

  val layout = SeatLayout(entries.map(_.toCharArray.toList).toList)
  println("Part 1: " + solve(layout, _.adjacent, 4))
  println("Part 2: " + solve(layout, _.withinLineOfSight, 5))
}