package solver

import parser.ast._
import tokenizer.tokenizer.tokenize
import rule._

object solver {
  def solve(string: String): String = {
    initSolve(parser.parser.parseExp(tokenize(string))._1).string(0)
  }

  def initSolve(exp: Exp): Rule = solveBody(exp, List())
  def solveBody(exp: Exp, env: Env): Rule = {
    exp match {
      case IntVal(n) =>
        EInt(env, IntVal(n))
      case BoolVal(b) =>
        EBool(env, BoolVal(b))
      case Var(n) =>
        if (env.head._1 == n) {
          EVar1(env, Var(n))
        } else {
          EVar2(env, Var(n), solveBody(Var(n), env.tail))
        }
      case InfixExp(e1, Plus, e2) =>
        val r1 = solveBody(e1, env)
        val r2 = solveBody(e2, env)
        val i3 = r1.value.asInstanceOf[IntVal] + r2.value
          .asInstanceOf[IntVal]
        EPlus(env, e1, e2, i3, r1, r2, BPlus(r1.value, r2.value, i3))
      case InfixExp(IntVal(0), Minus, e2) =>
        val r2 = solveBody(e2, env)
        val i3 = IntVal(
          -1 * r2.value
            .asInstanceOf[IntVal]
            .value
        )
        solveBody(i3, env)
      case InfixExp(e1, Minus, e2) =>
        val r1 = solveBody(e1, env)
        val r2 = solveBody(e2, env)
        val i3 = r1.value.asInstanceOf[IntVal] - r2.value
          .asInstanceOf[IntVal]
        EMinus(env, e1, e2, i3, r1, r2, BMinus(r1.value, r2.value, i3))
      case InfixExp(e1, Asterisk, e2) =>
        val r1 = solveBody(e1, env)
        val r2 = solveBody(e2, env)
        val i3 = r1.value.asInstanceOf[IntVal] * r2.value
          .asInstanceOf[IntVal]
        ETimes(env, e1, e2, i3, r1, r2, BTimes(r1.value, r2.value, i3))
      case InfixExp(e1, LessThan, e2) =>
        val r1 = solveBody(e1, env)
        val r2 = solveBody(e2, env)
        val i3 = r1.value.asInstanceOf[IntVal] < r2.value
          .asInstanceOf[IntVal]
        ELt(env, e1, e2, i3, r1, r2, BLt(r1.value, r2.value, i3))
      case IfExp(condExp, thenExp, elseExp) =>
        val r1 = solveBody(condExp, env)
        if (r1.value.asInstanceOf[BoolVal].value) {
          val r2 = solveBody(thenExp, env)
          EIfT(env, condExp, thenExp, elseExp, r1, r2)
        } else {
          val r3 = solveBody(elseExp, env)
          EIfF(env, condExp, thenExp, elseExp, r1, r3)
        }
      case LetExp(variable, valueExp, inExp) =>
        val r1 = solveBody(valueExp, env)
        val r2 = solveBody(inExp, (variable.name, r1.value) :: env)
        ELet(env, variable, valueExp, inExp, r1, r2)
      case _ =>
        throw new Exception("未対応")
    }
  }
}
