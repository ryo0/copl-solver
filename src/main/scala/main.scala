import parser.ast._
import solver.initSolve
import solver.solve
import tokenizer.tokenizer.tokenize
import parser.parser.parseExp

object main extends App {
  println(initSolve(parseExp(tokenize("3 + 5"))._1))
  println(initSolve(parseExp(tokenize("8 - 2 - 3"))._1))
  println(initSolve(parseExp(tokenize("(4 + 5) * (1 - 10) "))._1))
  println(initSolve(parseExp(tokenize("(4 + 5) > (1 - 10) "))._1))
  println("--------")
  println(initSolve(parseExp(tokenize("if 4 < 5 then 2 + 3 else 8 * 8"))._1))
  println("--------")
  println(initSolve(parseExp(tokenize("3 + if -23 < -2 * 8 then 8 else 2 + 4 "))._1))
  println("--------")
  println(initSolve(parseExp(tokenize("3 + (if -23 < -2 * 8 then 8 else 2) + 4 "))._1))
  println("--------")
  println(solve(parseExp(tokenize("x"))._1, List(("y", IntVal(2)), ("x", IntVal(3)))))
  println("--------")
  println(solve(parseExp(tokenize("if x then y + 1 else y - 1"))._1, List(("y", IntVal(4)), ("x", BoolVal(true)))))
  println("--------")


}
