import solver.solver.solve

object main extends App {
  println(solve("3 + 5"))
  println(solve("8 - 2 - 3"))
  println(solve("(4 + 5) * (1 - 10)"))
  println(solve("1+1 < (10-7)"))
  println(solve("if 4 < 5 then 2 + 3 else 8 * 8"))
  println(solve("3 + if -23 < -2 * 8 then 8 else 2 + 4"))
  println(solve("3 + (if -23 < -2 * 8 then 8 else 2) + 4"))
}
