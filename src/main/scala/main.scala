import parser.ast._
import solver.solver.solve
import tokenizer.tokenizer.tokenize
import parser.parser.parseExp
import java.io.PrintWriter

object main extends App {
  println(solve(parseExp(tokenize("3 + 5"))._1).string())
  println(solve(parseExp(tokenize("8 - 2 - 3"))._1).string())

}
