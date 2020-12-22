import scala.annotation.tailrec
import scala.io.Source

object Day21 extends App {
  def entries = Source.fromResource("day21.txt").getLines.map {
    case s"$ingredients (contains $allergens)" => Food(
      ingredients.split("\\s").toSet,
      allergens.split(",\\s").toSet
    )
  }

  case class Food(ingredients: Set[String], allergens: Set[String])

  val maybeAllergenic = entries
    .foldLeft(Map.empty[String, Set[String]]) { case (acc, curr) =>
      val pairs = curr.allergens.map(a =>
        if (acc.contains(a)) a -> acc(a).intersect(curr.ingredients)
        else a -> curr.ingredients
      )
      acc ++ pairs
    }

  def solvePart1 = entries
    .map(_
      .ingredients
      .diff(maybeAllergenic.values.reduceLeft(_ union _))
      .size
    )
    .sum

  @tailrec
  def solvePart2(
    maybeAllergenic: Map[String, Set[String]],
    solution: Map[String, String],
  ): String = maybeAllergenic match {
    case m if m.isEmpty => solution.toList.sortBy(_._2).map(_._1).mkString(",")
    case m =>
      // If there's a single ingredient in the set, we know it's
      // the one that contains the allergen.
      val (single, multiple) = m.partition(_._2.size == 1)
      solvePart2(
        // An ingredient can only contain one allergen.
        multiple.view.mapValues(_.filterNot(solution.contains)).toMap,
        solution ++ single.view.mapValues(_.head).map(_.swap)
      )
  }

  println(solvePart1)
  println(solvePart2(maybeAllergenic, Map.empty))
}