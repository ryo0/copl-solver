package eval
import parser.ast._

object eval {
  def eval(exp: Exp): Exp = {
    exp match {
      case IntVal(n) => IntVal(n)
      case IfExp(condExp, thenExp, elseExp) => {
        if (eval(condExp).asInstanceOf[BoolVal].value) {
          eval(thenExp)
        } else {
          eval(elseExp)
        }
      }
      case InfixExp(left, op, right) =>
        val leftVal = eval(left)
        val rightVal = eval(right)
        op match {
          case Plus =>
            plus(leftVal, rightVal)
          case Minus =>
            minus(leftVal, rightVal)
          case Asterisk =>
            times(leftVal, rightVal)
          case Slash =>
            div(leftVal, rightVal)
          case LessThan =>
            BoolVal(leftVal.asInstanceOf[IntVal].value < rightVal.asInstanceOf[IntVal].value)
          case GreaterThan =>
            BoolVal(leftVal.asInstanceOf[IntVal].value > rightVal.asInstanceOf[IntVal].value)
        }
    }
  }
  def plus(exp1: Exp, exp2: Exp): IntVal = {
    IntVal(exp1.asInstanceOf[IntVal].value + exp2.asInstanceOf[IntVal].value)
  }
  def minus(exp1: Exp, exp2: Exp): IntVal = {
    IntVal(exp1.asInstanceOf[IntVal].value - exp2.asInstanceOf[IntVal].value)
  }
  def times(exp1: Exp, exp2: Exp): IntVal = {
    IntVal(exp1.asInstanceOf[IntVal].value * exp2.asInstanceOf[IntVal].value)
  }
  def div(exp1: Exp, exp2: Exp): IntVal = {
    IntVal(exp1.asInstanceOf[IntVal].value / exp2.asInstanceOf[IntVal].value)
  }
}
