package joe.aoc

object Day7 extends AocApp(7) {

  private val cardValues = Seq('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J').reverse.zipWithIndex.toMap

  case class Hand(cards: String) {
    val strength: Int =
      val bob = cards.toCharArray.toSeq.groupBy(identity).map { case (card, grouped) => card -> grouped.size }
      val jCount = bob.getOrElse('J', 0)
      val remainder = bob.removed('J').values.toSeq
      val sorted = remainder.sorted.reverse
      val withWildcard = if sorted.nonEmpty then (sorted.head + jCount) +: sorted.tail else sorted

      withWildcard match {
        case Seq(5) => 7
        case Seq(4, 1) => 6
        case Seq(3, 2) => 5
        case Seq(3, 1, 1) => 4
        case Seq(2, 2, 1) => 3
        case Seq(2, 1, 1, 1) => 2
        case Seq(1, 1, 1, 1, 1) => 1
        case Seq() => 7 // JJJJJ
      }

    def relativeStrength(other: Hand): Boolean =
      val matched = this.asScores.zip(other.asScores).find(_ != _)
      matched.exists(_ < _)

    val asScores: Seq[Int] = cards.map(cardValues)
  }
  case class Bid(value: Int)
  case class Game(hand: Hand, bid: Bid)

  override def part1(input: Seq[String]): Int =
    val games: Seq[Game] = input.map: line =>
      val Array(l, r) = line.split(" ")
      Game(Hand(l), Bid(r.toInt))
      // first before second => true
    val sorted = games.sortWith: (l, r) =>
      val lStrength = l.hand.strength
      val rStrength = r.hand.strength
      if lStrength == rStrength then
        l.hand.relativeStrength(r.hand)
      else
        lStrength < rStrength

    sorted.zipWithIndex.map: (game, rank) =>
      game.bid.value * (rank + 1)
    .sum


  override def part2(input: Seq[String]): Int = {
    -1
  }

}

@main def run7(): Unit = Day7.run()