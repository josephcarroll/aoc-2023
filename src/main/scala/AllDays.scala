package joe.aoc

object AllDays extends App {

  val days: Seq[App] = Seq(Day1)
  days.zipWithIndex.foreach { case (app, i) =>
    println()
    val header = s"Day ${i + 1}"
    println("=" * header.length)
    println(header)
    println("=" * header.length)
    println()
    app.main(Array())
  }

}
