package solver

import parser.ast._
import rule._

object solver {
  def solve(exp: Exp): Rule = {
    exp match {
      case IntVal(n) =>
        EInt(IntVal(n))
      case BoolVal(b) =>
        EBool(BoolVal(b))
      case InfixExp(e1, Plus, e2) =>
        val r1 = solve(e1)
        val r2 = solve(e2)
        val i3 = r1.value().asInstanceOf[IntVal] + r2
          .value()
          .asInstanceOf[IntVal]
        EPlus(e1, e2, i3, r1, r2, BPlus(r1.value(), r2.value(), i3))
      case InfixExp(e1, Minus, e2) =>
        val r1 = solve(e1)
        val r2 = solve(e2)
        val i3 = r1.value().asInstanceOf[IntVal] - r2
          .value()
          .asInstanceOf[IntVal]
        EMinus(e1, e2, i3, r1, r2, BMinus(r1.value(), r2.value(), i3))
      case InfixExp(e1, Asterisk, e2) =>
        val r1 = solve(e1)
        val r2 = solve(e2)
        val i3 = r1.value().asInstanceOf[IntVal] * r2
          .value()
          .asInstanceOf[IntVal]
        ETimes(e1, e2, i3, r1, r2, BTimes(r1.value(), r2.value(), i3))
      case InfixExp(e1, LessThan, e2) =>
        val r1 = solve(e1)
        val r2 = solve(e2)
        val i3 = r1.value().asInstanceOf[IntVal] < r2
          .value()
          .asInstanceOf[IntVal]
        ELt(e1, e2, i3, r1, r2, BLt(r1.value(), r2.value(), i3))
      case _ =>
        throw new Exception("未対応")
    }
  }
}
