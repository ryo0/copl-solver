import solver.initSolve
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

}
