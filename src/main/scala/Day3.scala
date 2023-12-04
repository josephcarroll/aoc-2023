package joe.aoc

import scala.util.Try

object Day3 extends App {

  case class GridNumber(x: Int, y: Int, value: Int) {
    def adjacencyList: Seq[(Int, Int)] = {
      for (
        c <- 0 until value.toString.length;
        i <- -1 to 1;
        j <- -1 to 1
      ) yield {
        (x + c + i, y + j)
      }
    }
  }
  case class GridSymbol(x: Int, y: Int, value: String) {
    def adjacencyList: Seq[(Int, Int)] = {
      for (
        i <- -1 to 1;
        j <- -1 to 1
      ) yield {
        (x + i, y + j)
      }
    }
  }

  def symbols(input: Seq[String]): Int = {
    val numbers = input.zipWithIndex.flatMap { case (line, index) =>
      val matcher = """(\d+)""".r
      matcher.findAllMatchIn(line).map { m =>
        GridNumber(m.start, index, m.group(0).toInt)
      }
    }

    val symbols = input.zipWithIndex.flatMap { case (line, index) =>
      val matcher = """([^\d.])""".r
      matcher.findAllMatchIn(line).map { m =>
        GridSymbol(m.start, index, m.group(0))
      }
    }

    val symbolSet = symbols.map(s => (s.x, s.y)).toSet

    val adjacentNumbers = numbers.filter(n => n.adjacencyList.exists(c => symbolSet.contains(c)))
    adjacentNumbers.map(_.value).sum
  }

  def gears(input: Seq[String]): Int = {
    val numbers = input.zipWithIndex.flatMap { case (line, index) =>
      val matcher = """(\d+)""".r
      matcher.findAllMatchIn(line).map { m =>
        GridNumber(m.start, index, m.group(0).toInt)
      }
    }

    val symbols = input.zipWithIndex.flatMap { case (line, index) =>
      val matcher = """([^\d.])""".r
      matcher.findAllMatchIn(line).map { m =>
        GridSymbol(m.start, index, m.group(0))
      }
    }

    val numberSet = numbers.flatMap { n =>
      for (c <- 0 until n.value.toString.length) yield (n.x + c, n.y) -> n
    }.toMap

    symbols.filter(_.value == "*").flatMap { symbol =>
      val matched = symbol.adjacencyList.collect {
        case location if numberSet.contains(location) => numberSet(location)
      }.toSet
      if (matched.size > 1) Some(matched.map(_.value).product) else None
    }.sum
  }

  println(symbols(Helpers.sample(3)))
  println(symbols(Helpers.input(3)))
  println(gears(Helpers.sample(3)))
  println(gears(Helpers.input(3)))

}