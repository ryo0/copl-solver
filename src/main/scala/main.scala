import solver.solve
import tokenizer.tokenizer.tokenize
import parser.parser.parseExp

object main extends App {
  println(solve(parseExp(tokenize("3 + 5"))._1))
  println(solve(parseExp(tokenize("8 - 2 - 3"))._1))
  println(solve(parseExp(tokenize("(4 + 5) * (1 - 10) "))._1))

}
