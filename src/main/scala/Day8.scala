package joe.aoc

object Day8 extends AocApp(8) {

  case class Node(name: String, left: String, right: String)

  override def part1(input: Seq[String]): Int = {
    val instructions = input.head.toCharArray
    val nodes = input.tail.tail.map { line =>
      val matcher = """([A-Z]+)""".r
      val Array(a, b, c) = matcher.findAllMatchIn(line).map(_.group(0)).toArray
      Node(a, b, c)
    }.map(n => n.name -> n).toMap

    val start = nodes("AAA")
    val end = nodes("ZZZ")
    var current = start
    var count = 0
    while (current != end) {
      val instruction = instructions(count % instructions.length)
      if (instruction == 'L') {
        current = nodes(current.left)
      } else {
        current = nodes(current.right)
      }
      count +=1
    }

    count
  }

  override def part2(input: Seq[String]): Int = {
    val instructions = input.head.toCharArray
    val nodes = input.tail.tail.map { line =>
      val matcher = """([A-Z]+)""".r
      val Array(a, b, c) = matcher.findAllMatchIn(line).map(_.group(0)).toArray
      Node(a, b, c)
    }.map(n => n.name -> n).toMap

    val starters = nodes.values.filter(_.name.endsWith("A"))
    starters.foreach { start =>
      var current = start
      var count = 0
      while (!current.name.endsWith("Z")) {
        val instruction = instructions(count % instructions.length)
        if (instruction == 'L') {
          current = nodes(current.left)
        } else {
          current = nodes(current.right)
        }
        count += 1
      }
      println(count)
      println(current)
    }

    -1
  }

}

@main def run8(): Unit = Day8.run()