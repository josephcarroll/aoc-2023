package joe.aoc
import scala.util.parsing.combinator.RegexParsers

object Day2 extends AocApp(2) {

  case class Game(id: Int, rounds: Seq[Round])
  case class Round(count: Int, colour: String)

  object GameParser extends RegexParsers {

    def round: Parser[Round] = """\d+""".r ~ """\w+""".r ^^ {
      case count ~ colour => Round(count.toInt, colour)
    }

    def game: Parser[Game] = "Game" ~ """\d+""".r ~ ":" ~ repsep(round, "[,;]+".r) ^^ {
      case _ ~ id ~ _ ~ rounds => Game(id.toInt, rounds)
    }

    def parse(input: String): Game = parse(game, input).get

  }

  private val knownBag = Map(
    "red" -> 12,
    "blue" -> 14,
    "green" -> 13
  )

  override def part1(input: Seq[String]): Int = {
    val matching = input.flatMap { line =>
      val game = GameParser.parse(line)
      val ok = game.rounds.forall { round =>
        knownBag(round.colour) >= round.count
      }
      if ok then Some(game.id) else None
    }
    matching.sum
  }

  override def part2(input: Seq[String]): Int = {
    val matching = input.map { line =>
      val game = GameParser.parse(line)
      val minRequirements = game.rounds.groupBy(_.colour).map { case (_, rounds) =>
        rounds.map(_.count).max
      }
      minRequirements.product
    }
    matching.sum
  }

}

@main def run2(): Unit = Day2.run()