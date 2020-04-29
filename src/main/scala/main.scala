import solver.solver.solve

object main extends App {
  println(solve("3 + 5"))
  println(solve("8 - 2 - 3"))
  println(solve("(4 + 5) * (1 - 10)"))
  println(solve("1+1 < (10-7)"))
  println(solve("if 4 < 5 then 2 + 3 else 8 * 8"))
  println(solve("3 + if -23 < -2 * 8 then 8 else 2 + 4"))
  println(solve("3 + (if -23 < -2 * 8 then 8 else 2) + 4"))
  println(solve("let x = 3 in let y = 2 in x"))
  println(solve("let x = true in let y = 4 in if x then y + 1 else y - 1"))
  println(solve("let x = 1 + 2 in x * 4"))
  println(solve("let x = 3 * 3 in let y = 4 * x in x + y"))
  println(solve("let x = 3 in let x = x * 2 in x + x"))
  println(solve("let x = let y = 3 - 2 in y * y in let y = 4 in x + y"))
  println(solve("fun x -> x + 1"))
  println(solve("let y = 2 in fun x -> x + y"))
  println(solve("let sq = fun x -> x * x in sq 3 + sq 4"))

}
