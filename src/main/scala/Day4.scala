import scala.annotation.tailrec
import scala.io.Source

object Day4 extends App {
  def entries = Source.fromResource("day4.txt").getLines

  val required = Set(
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid",
  )

  val eyeColors = List(
    "amb",
    "blu",
    "brn",
    "gry",
    "grn",
    "hzl",
    "oth",
  )

  type Passport = Map[String, String]

  @tailrec
  def passports(entries: Iterator[String], acc: List[Passport] = Nil): List[Passport] = {
    if (entries.isEmpty) acc
    else {
      val (passport, rest) = entries.span(_.nonEmpty)
      passports(
        // The first line is empty.
        rest.drop(1),
        processPassport(passport) +: acc
      )
    }
  }

  def processPassport(passport: Iterator[String]): Passport = passport
    .mkString(" ")
    .split(" ")
    .map { case s"$field:$value" => (field, value) }
    .toMap

  def isValid(passport: Passport): Boolean =
    List(
      (1920 to 2002).contains(passport("byr").toInt),
      (2002 to 2020).contains(passport("iyr").toInt),
      (2020 to 2030).contains(passport("eyr").toInt),

      {
        val (num, unit) = passport("hgt").span(_.isDigit)
        val range = if (unit == "cm") 150 to 193 else 59 to 76
        range.contains(num.toInt)
      },

      "#[0-9a-f]{6}".r.matches(passport("hcl")),
      "\\d{9}".r.matches(passport("pid")),
      eyeColors.contains(passport("ecl")),
    ).forall(identity)

  def passportsWithRequiredFields = passports(entries).filter(p => required.subsetOf(p.keySet))

  println("Part 1: " + passportsWithRequiredFields.length)
  println("Part 2: " + passportsWithRequiredFields.count(isValid))
}