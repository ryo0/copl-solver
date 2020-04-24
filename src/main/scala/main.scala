import parser.ast._
import solver.initSolve
import solver.solve
import tokenizer.tokenizer.tokenize
import parser.parser.parseExp

object main extends App {
  println(parseExp(tokenize("3 + 5"))._1)
  println(initSolve(parseExp(tokenize("3 + 5"))._1))
  println(initSolve(parseExp(tokenize("8 - 2 - 3"))._1))
  println(initSolve(parseExp(tokenize("(4 + 5) * (1 - 10) "))._1))
  println(initSolve(parseExp(tokenize("(4 + 5) > (1 - 10) "))._1))
  println("--------")
  println(initSolve(parseExp(tokenize("if 4 < 5 then 2 + 3 else 8 * 8"))._1))
  println("--------")
  println(
    initSolve(parseExp(tokenize("3 + if -23 < -2 * 8 then 8 else 2 + 4 "))._1)
  )
  println("--------")
  println(
    initSolve(parseExp(tokenize("3 + (if -23 < -2 * 8 then 8 else 2) + 4 "))._1)
  )
  println("--------")
  println(
    solve(parseExp(tokenize("x"))._1, List(("y", IntVal(2)), ("x", IntVal(3))))
  )
  println("--------")
  println(
    solve(
      parseExp(tokenize("if x then y + 1 else y - 1"))._1,
      List(("y", IntVal(4)), ("x", BoolVal(true)))
    )
  )
  println("--------")
  println(solve(parseExp(tokenize("let x = 1 + 2 in x * 4"))._1, List()))
  println("--------")
  println(
    solve(
      parseExp(tokenize("let x = 3 * 3 in let y = 4 * x in x + y"))._1,
      List()
    )
  )
  println("--------")
  println(
    solve(
      parseExp(tokenize("let x = x * 2 in x + x"))._1,
      List(("x", IntVal(3)))
    )
  )
  println("--------")
  println(
    solve(
      parseExp(tokenize("let x = let y = 3 - 2 in y * y in let y = 4 in x + y"))._1,
      List()
    )
  )
  println("--------")
  println(solve(parseExp(tokenize("fun x -> x + 1"))._1, List()))
  println("--------")
  println(
    solve(
      parseExp(tokenize("let sq = fun x -> x * x in sq 3 + sq 4 "))._1,
      List()
    )
  )
  println("--------")
  println(
    solve(
      parseExp(tokenize("let sq = fun x -> x * x in sq 3 + sq 4 "))._1,
      List()
    )
  )
  println("--------")
  println(
    solve(
      parseExp(tokenize(" let sm = fun f -> f 3 + f 4 in sm (fun x -> x * x)"))._1,
      List()
    )
  )
  println("--------")
  println(
    solve(
      parseExp(
        tokenize("let a = 3 in let f = fun y -> y * a in let a = 5 in f 4")
      )._1,
      List()
    )
  )

  println("--------")
  println(
    solve(parseExp(tokenize("let a = 1 in let b = 2 in fun x -> 1"))._1, List())
  )
  println("--------")
  println(
    solve(
      parseExp(tokenize("let f = fun x -> fun y -> x + y in f 1 2"))._1,
      List()
    )
  )
}
