package solver
import parser.ast._
import tokenizer.tokenizer.tokenize
import typeRule._
object typeSolver {
  def typeSolve(string: String): String = {
    initSolve(parser.parser.parseExp(tokenize(string))._1).string(0)
  }

  def typeSolveInEnv(typeEnv: TypeEnv, string: String): String = {
    val exp = parser.parser.parseExp(tokenize(string))._1
    val solved = exp.typeSolve(typeEnv)
    solved.string(0)
  }
  def initSolve(exp: Exp): TypeRule = { exp.typeSolve(List()) }

}
