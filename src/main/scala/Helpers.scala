package joe.aoc

import scala.annotation.tailrec

object Helpers {

  def sample(day: Int): Seq[String] = readInput(day, "sample")
  def sample2(day: Int): Seq[String] = readInput(day, "sample2")

  def input(day: Int): Seq[String] = readInput(day, "input")

  def readInput(day: Int, suffix: String): Seq[String] = {
    val name = s"day$day-$suffix.txt"
    io.Source.fromResource(name).getLines().toSeq
  }

  def readEntireInput(day: Int): String = {
    readInput(day, "input").mkString("\n")
  }

  def splitBySentinel[T](input: Seq[T], sentinelValue: T): Seq[Seq[T]] = {
    @tailrec def split(result: Seq[Seq[T]], remainder: Seq[T]): Seq[Seq[T]] = {
      if remainder.isEmpty then {
        result
      } else {
        val (l, r) = remainder.span(_ != sentinelValue)
        split(result :+ l, r.drop(1))
      }
    }
    split(Seq(), input)
  }

}
