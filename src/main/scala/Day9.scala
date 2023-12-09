package joe.aoc

import scala.annotation.tailrec

object Day9 extends AocApp(9) {

  override def part1(input: Seq[String]): Int = {
    def interpolate(numbers: Seq[Int]): Int = {
      if (numbers.forall(_ == 0)) {
        0
      } else {
        val diffs = numbers.sliding(2).toSeq.map { case Seq(a, b) => b - a }
        numbers.last + interpolate(diffs)
      }
    }

    val numbers = input.map(_.split(" ").map(_.toInt).toSeq)
    val interpolated = numbers.map(interpolate)
    interpolated.sum
  }

  override def part2(input: Seq[String]): Int = {
    def interpolate(numbers: Seq[Int]): Int = {
      if (numbers.forall(_ == 0)) {
        0
      } else {
        val diffs = numbers.sliding(2).toSeq.map { case Seq(a, b) => b - a }
        numbers.head - interpolate(diffs)
      }
    }

    val numbers = input.map(_.split(" ").map(_.toInt).toSeq)
    val interpolated = numbers.map(interpolate)
    interpolated.sum
  }

}

@main def run9(): Unit = Day9.run()