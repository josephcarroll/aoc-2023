package joe.aoc

import scala.annotation.tailrec

object Day10 extends AocApp(10) {

  case class Coordinate(x: Int, y: Int) {
    def adjacent(xBounds: Int, yBounds: Int): Seq[Coordinate] = {
      def inBounds(c: Coordinate) = c.x >= 0 && c.x < xBounds && c.y >= 0 && c.y < yBounds
      Seq(
        Coordinate(x - 1, y),
        Coordinate(x + 1, y),
        Coordinate(x, y - 1),
        Coordinate(x, y + 1),
      ).filter(inBounds)
    }
  }

  case class Node(char: Char, c: Coordinate) {
    def isNext(from: Node): Boolean = from.char match {
      case '|' if from.c.x == c.x => true // is a vertical pipe connecting north and south.
      case '-' if from.c.y == c.y => true // is a horizontal pipe connecting east and west.
      case 'L' if (from.c.x == c.x && from.c.y - 1 == c.y) || (from.c.y == c.y && from.c.x + 1 == c.x) => true // is a 90 - degree bend connecting north and east.
      case 'J' if (from.c.x == c.x && from.c.y - 1 == c.y) || (from.c.y == c.y && from.c.x - 1 == c.x) => true // is a 90 - degree bend connecting north and west.
      case '7' if (from.c.x == c.x && from.c.y + 1 == c.y) || (from.c.y == c.y && from.c.x - 1 == c.x) => true // is a 90 - degree bend connecting south and west.
      case 'F' if (from.c.x == c.x && from.c.y + 1 == c.y) || (from.c.y == c.y && from.c.x + 1 == c.x) => true // is a 90 - degree bend connecting south and east.
      case _   => false
    }
  }

  def createPath(input: Seq[String]): Seq[Node] = {
    val xBounds = input.length
    val yBounds = input.head.length

    val allNodes = for
      i <- input.indices
      j <- input.head.indices
    yield
      val currentNode = input(j)(i)
      Coordinate(i, j) -> Node(currentNode, Coordinate(i, j))
    val nodeMap = allNodes.toMap
    val start = nodeMap.values.find(_.char == 'S').get

    @tailrec
    def path(currentPath: Seq[Node]): Seq[Node] = {
      val current = currentPath.last
      val adjacent = current.c.adjacent(xBounds, yBounds).map(nodeMap)
      val nextOptions = adjacent.filter(_.isNext(current))
      val next = nextOptions.filter(n => !currentPath.contains(n))
      next match {
        case Nil => if adjacent.contains(start) then currentPath else Nil
        case Seq(n) => path(currentPath :+ n)
      }
    }

    val startPoints = start.c.adjacent(xBounds, yBounds).map(nodeMap)

    val paths = startPoints.filter(p => start.isNext(p)).map(s => path(Seq(start, s)))
    paths.filter(_.nonEmpty).head
  }

  override def part1(input: Seq[String]): Int = {
    val result = createPath(input)
    result.length / 2
  }

  override def part2(input: Seq[String]): Int = {
    val path = createPath(input)
    val pathSet = path.map(n => n.c -> n).toMap
    val counts = for
      i <- input.indices
      j <- input.head.indices
      if !pathSet.contains(Coordinate(i, j))
    yield
      val intersections = Range(0, j).flatMap(jDelta => pathSet.get(Coordinate(i, jDelta)))
      intersections.count(n => Set('-', 'J', '7').contains(n.char))

    counts.count(_ % 2 != 0)
  }

}

@main def run10(): Unit = Day10.run()