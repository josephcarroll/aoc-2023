package joe.aoc

trait AocApp(day: Int, twoSamples: Boolean = false) {

  def part1(input: Seq[String]): Int
  def part2(input: Seq[String]): Int

  def run(): Unit = {
    println(part1(Helpers.sample(day)))
    println(part1(Helpers.input(day)))
    val part2Sample = if twoSamples then Helpers.sample2(day) else Helpers.sample(day)
    println(part2(part2Sample))
    println(part2(Helpers.input(day)))
  }

}
