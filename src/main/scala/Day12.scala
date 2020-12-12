import scala.io.Source

object Day12 extends App {
  def entries = Source.fromResource("day12.txt").getLines

  def instructions = entries
    .map(_.span(_.isLetter))
    .map {
      case (dir, value) => Instruction(dir, value.toInt)
    }

  case class Ship(position: Position, facing: String) {
    def update(instruction: Instruction): Ship =
      instruction.action match {
        case "N" =>
          copy(position = position.withYOffset(instruction.value))
        case "S" =>
          copy(position = position.withYOffset(-instruction.value))
        case "E" =>
          copy(position = position.withXOffset(instruction.value))
        case "W" =>
          copy(position = position.withXOffset(-instruction.value))
        case "L" =>
          copy(facing = rotate(360 - instruction.value))
        case "R" =>
          copy(facing = rotate(instruction.value))
        case "F" =>
          update(Instruction(facing, instruction.value))
      }

    /**
     * Get the direction we face after rotating by N degrees,
     * where N is a multiple of 90.
     */
    def rotate(angle: Int): String = LazyList
      .continually(List("N", "E", "S", "W"))
      .flatten
      .dropWhile(_ != facing)(angle / 90)
  }
  object Ship {
    def default: Ship = Ship(Position(0, 0), "E")
  }

  /**
   * A waypoint.
   * @param position The waypoint's position relative to a ship.
   */
  case class Waypoint(position: Position) {
    def update(instruction: Instruction): Waypoint =
      instruction.action match {
        case "N" =>
          copy(position = position.withYOffset(instruction.value))
        case "S" =>
          copy(position = position.withYOffset(-instruction.value))
        case "E" =>
          copy(position = position.withXOffset(instruction.value))
        case "W" =>
          copy(position = position.withXOffset(-instruction.value))
        case "L" =>
          copy(position = rotate(360 - instruction.value))
        case "R" =>
          copy(position = rotate(instruction.value))
      }

    def rotate(angle: Int): Position =
      (1 to angle / 90).foldLeft(position) {
        case (Position(x, y), _) => Position(y, -x)
      }
  }
  object Waypoint {
    def default: Waypoint = Waypoint(Position(10, 1))
  }

  case class Instruction(action: String, value: Int)
  case class Position(x: Int, y: Int) {
    def withXOffset(value: Int): Position = copy(x = x + value)
    def withYOffset(value: Int): Position = copy(y = y + value)
  }

  def solvePart1: Int = {
    val Ship(position, _) = instructions.foldLeft(Ship.default)(_ update _)
    position.x.abs + position.y.abs
  }

  def solvePart2: Int = {
    val (_, Ship(position, _)) = instructions.foldLeft((Waypoint.default, Ship.default)) {
      case ((waypoint, ship), curr) =>
        if (curr.action == "F") (
          waypoint,
          ship.copy(position = ship.position
            .withXOffset(waypoint.position.x * curr.value)
            .withYOffset(waypoint.position.y * curr.value)
          )
        )
        else (waypoint.update(curr), ship)
    }
    position.x.abs + position.y.abs
  }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}