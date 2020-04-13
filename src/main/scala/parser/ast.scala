package parser

object ast {
  sealed class Op
  object Plus        extends Op
  object Minus       extends Op
  object Asterisk    extends Op
  object Slash       extends Op
  object GreaterThan extends Op
  object LessThan    extends Op

  sealed class Exp
  case class IntVal(value: Int) extends Exp
  case class BoolVal(value: Boolean) extends Exp
  case class InfixExp(leftExp: Exp, op: Op, rightExp: Exp) extends Exp
  case class IfExp(condExp: Exp, thenExp: Exp, elseExp: Exp) extends Exp
}
