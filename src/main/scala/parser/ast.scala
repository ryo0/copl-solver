package parser

object ast {

  sealed class Op
  object Plus        extends Op
  object Minus       extends Op
  object Asterisk    extends Op

  sealed class Exp
  case class IntVal(value: Int) extends Exp
  case class InfixExp(leftExp: Exp, op: Op, rightExp: Exp) extends Exp
}
