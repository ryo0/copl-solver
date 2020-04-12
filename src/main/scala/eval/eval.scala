package eval
import parser.ast._

object eval {
  def eval(exp: Exp): Int = {
    exp match {
      case IntVal(n) => n
      case InfixExp(left, op, right) =>
        op match {
          case Plus =>
            eval(left) + eval(right)
          case Minus =>
            eval(left) - eval(right)
          case Asterisk =>
            eval(left) * eval(right)
        }
    }
  }
}
