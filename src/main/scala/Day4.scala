package joe.aoc

import scala.util.parsing.combinator.JavaTokenParsers

object Day4 extends App {

  case class Card(id: Int, winners: Seq[Int], picked: Seq[Int]) {
    def winnerCount: Int = winners.toSet.intersect(picked.toSet).size
  }
  object CardParser extends JavaTokenParsers {

    def card: Parser[Card] = "Card" ~ wholeNumber ~ ":" ~ rep(wholeNumber) ~ "|" ~ rep(wholeNumber) ^^ {
      case _ ~ id ~ _ ~ winners ~ _ ~ picked =>
        Card(id.toInt, winners.map(_.toInt), picked.map(_.toInt))
    }

    def parse(input: String): Card = parse(card, input).get

  }

  def check(input: Seq[String]): Int = {
    val cards = input.map(CardParser.parse)
    val scores = cards.map { card =>
      if (card.winnerCount == 0) 0 else (1 +: Seq.fill(card.winnerCount - 1)(2)).product
    }
    scores.sum
  }

  def newGame(input: Seq[String]): Int = {
    val cards = input.map(CardParser.parse)

    def winners(card: Card): Int = {
      val children = (0 until card.winnerCount).map(_ + card.id).map(cards)
      1 + children.map(winners).sum
    }

    val scores = cards.map(winners)
    scores.sum
  }

  println(check(Helpers.sample(4)))
  println(check(Helpers.input(4)))
  println(newGame(Helpers.sample(4)))
  println(newGame(Helpers.input(4)))

}