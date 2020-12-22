import scala.io.Source

object Day14 extends App {
  def entries: Iterator[Instruction] = Source.fromResource("day14.txt").getLines.map {
    case s"mask = $mask" => Mask(mask)
    case s"mem[$address] = $value" => Assignment(address.toInt, value.toInt)
  }

  /** Convert an integer to a 0-padded binary string. */
  def toBinary(x: Int) =
    x.toBinaryString.reverse.padTo(36, '0').reverse

  def parseLong(s: String) = java.lang.Long.parseLong(s, 2)

  trait Instruction

  case class Mask(value: String) extends Instruction {
    def maskValue(x: Int): Long = parseLong(
      value
        .zip(toBinary(x))
        .map { case (m, b) => if (m == 'X') b else m }
        .mkString
    )

    def maskAddress(x: Int): List[Long] = {
      val masked = value.zip(toBinary(x)).map { case (m, b) =>
        if (m == '0') b
        else if (m == '1') '1'
        else 'X'
      }
      val floating = masked.zipWithIndex.filter(_._1 == 'X').map(_._2).toList

      /** Get all variations of an address. */
      def float(x: String, floating: List[Int]): List[Long] = {
        floating match {
          case Nil => List(parseLong(x))
          case index :: rest =>
            List('0', '1').flatMap(v => float(x.updated(index, v), rest))
        }
      }

      float(masked.mkString, floating)
    }
  }

  case class Assignment(address: Int, value: Int) extends Instruction

  def solvePart1: Long = {
    val (_, mem) = entries.foldLeft((Mask(""), Map.empty[Int, Long])) {
      case ((_, mem), mask: Mask) => (mask, mem)
      case ((mask, mem), assign: Assignment) => (
        mask,
        mem + (assign.address -> mask.maskValue(assign.value))
      )
    }
    mem.values.sum
  }

  def solvePart2: Long = {
    val (_, mem) = entries.foldLeft((Mask(""), Map.empty[Long, Long])) {
      case ((_, mem), mask: Mask) => (mask, mem)
      case ((mask, mem), assign: Assignment) => (
        mask,
        mem ++ mask.maskAddress(assign.address).map(_ -> assign.value)
      )
    }
    mem.values.sum
  }

  println(solvePart1)
  println(solvePart2)
}