import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Day7 extends App {
  def entries = Source.fromResource("day7.txt").getLines

  val pattern = new Regex("(?:(\\d) )?(\\w+ \\w+) bag", "num", "color")

  val startColor = "shiny gold"

  val directChildren = entries
    .map(entry => pattern.findAllMatchIn(entry).toList)
    .map {
      case bag :: contents =>
        val pairs = contents
          // Filter out rules where a bag doesn't contain any other.
          .map(_.subgroups.filter(_ != null))
          .collect { case List(num, color) => color -> num.toInt }
          .toMap
        bag.group("color") -> pairs
    }
    .toMap

  val directParents: Map[String, List[String]] = directChildren
    .toList
    .flatMap { case (parent, children) => children.keys.map(_ -> parent) }
    .groupBy(_._1)
    .view.mapValues(_.map(_._2))
    .toMap

  @tailrec
  def getParents(explore: List[String], acc: Set[String] = Set.empty): Int =
    explore match {
      case child :: rest =>
        val parents = directParents.getOrElse(child, Nil)
        getParents(rest ++ parents, acc ++ parents)
      case Nil => acc.size
    }

  // TODO: See if we can make this tail recursive.
  def countChildren(curr: String): Int = directChildren
    .getOrElse(curr, Map.empty)
    .map { case (child, num) => countChildren(child) * num + num }
    .sum

  println("Part 1: " + getParents(List(startColor)))
  println("Part 2: " + countChildren(startColor))
}