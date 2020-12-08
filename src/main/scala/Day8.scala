import enumeratum.EnumEntry._
import enumeratum._

import scala.annotation.tailrec
import scala.io.Source

object Day8 extends App {
  def entries = Source.fromResource("day8.txt").getLines

  val instructions: List[Instruction] = entries
    .map { case s"$op $arg" => Instruction(Op.withName(op), arg.toInt) }
    .toList

  sealed trait Op extends EnumEntry with Lowercase

  object Op extends Enum[Op] {
    val values = findValues
    case object Acc extends Op
    case object Jmp extends Op
    case object Nop extends Op
  }

  case class Instruction(op: Op, arg: Int)

  case class State(index: Int, seen: Set[Int], acc: Int) {
    def apply(inst: Instruction): State = {
      inst.op match {
        case Op.Nop => State(index + 1, seen + index, acc)
        case Op.Acc => State(index + 1, seen + index, acc + inst.arg)
        case Op.Jmp => State(index + inst.arg, seen + index, acc)
      }
    }
  }

  object State {
    def default: State = State(0, Set.empty, 0)
  }

  @tailrec
  def solvePart1(state: State = State.default): Int = {
    if (state.seen.contains(state.index))
      state.acc
    else solvePart1(state(instructions(state.index)))
  }

  def solvePart2: Int = {

    @tailrec
    def execute(instructions: List[Instruction], state: State = State.default): Option[Int] = {
      // We're in a loop, so we modified the wrong instruction.
      if (state.seen.contains(state.index)) None
      else if (state.index >= instructions.length) Some(state.acc)
      else execute(instructions, state(instructions(state.index)))
    }

    (for {
      List(op, newOp) <- List(Op.Jmp, Op.Nop).permutations
      index <- instructions.zipWithIndex.collect {
        case (inst, index) if inst.op == op => index
      }
      acc = execute(instructions.updated(index, instructions(index).copy(op = newOp)))
      if acc.isDefined
    } yield acc.get).next
  }

  println("Part 1: " + solvePart1())
  println("Part 2: " + solvePart2)
}