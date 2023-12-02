package joe.aoc

object Day1 extends App {

  def numbers(input: Seq[String]): Int = {
    input.map { line =>
      val digits = line.filter(_.isDigit).map(_.toString)
      (digits(0) + digits.last).toInt
    }.sum
  }

  val characterMap = Map(
    "one" -> "1",
    "two" -> "2",
    "three" -> "3",
    "four" -> "4",
    "five" -> "5",
    "six" -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine" -> "9"
  )

  def spelt(input: Seq[String]): Int = {
    input.map { line =>
      val digits = line.zipWithIndex.flatMap {
        case (character, _) if character.isDigit =>
          Some(character.toString)
        case (_, index) =>
          val currentStart = line.substring(index)
          characterMap.collectFirst {
            case (string, digit) if currentStart.startsWith(string) => digit
          }
      }
      (digits(0) + digits.last).toInt
    }.sum
  }

  println(numbers(Helpers.sample(1)))
  println(numbers(Helpers.input(1)))
  println(spelt(Helpers.sample2(1)))
  println(spelt(Helpers.input(1)))

}