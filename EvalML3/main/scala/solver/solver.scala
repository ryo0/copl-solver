package solver

import parser.ast._
import tokenizer.tokenizer.tokenize
import rule._

object solver {
  def solve(string: String): String = {
    initSolve(parser.parser.parseExp(tokenize(string))._1).string(0)
  }

  def initSolve(exp: Exp): Rule = { exp.solve(List()) }

}
