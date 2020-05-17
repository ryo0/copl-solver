import solver.typeSolver.typeSolve
import java.io.PrintWriter

import parser.ast.{EList, EmptyList, IntVal, Var, WildCard}
import tokenizer.token.ElseToken

object main extends App {
  val file81 = new PrintWriter("TypingML4Result/1.txt")
  file81.write(typeSolve("3 + 5 "))
  file81.close()

}
