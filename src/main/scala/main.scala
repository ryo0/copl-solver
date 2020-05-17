import solver.typeSolver.typeSolve
import solver.typeSolver.typeSolveInEnv
import java.io.PrintWriter

import parser.ast.{EList, EmptyList, IntVal, Var, WildCard}
import solver.typeRule.{MLBoolType, MLIntType}
import tokenizer.token.ElseToken

object main extends App {
  val file81 = new PrintWriter("TypingML4Result/1.txt")
  file81.write(typeSolve("3 + 5"))
  file81.close()

  val file82 = new PrintWriter("TypingML4Result/2.txt")
  file82.write(typeSolve("if 4 < 5 then 2 + 3 else 8 * 8"))
  file82.close()

  val file83 = new PrintWriter("TypingML4Result/3.txt")
  file83.write(
    typeSolveInEnv(
      List(("y", MLIntType), ("x", MLBoolType)),
      "if x then y + 1 else y - 1"
    )
  )
  file83.close()

  val file84 = new PrintWriter("TypingML4Result/4.txt")
  file84.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
  file84.close()
}
