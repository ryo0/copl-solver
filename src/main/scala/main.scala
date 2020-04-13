import solver.solve
import tokenizer.tokenizer.tokenize
import parser.parser.parseSum

object main extends App {
  println(solve(parseSum(tokenize("3 + 5"))._1))
  println(solve(parseSum(tokenize("8 - 2 - 3"))._1))
  println(solve(parseSum(tokenize("(4 + 5) * (1 - 10) "))._1))

}
