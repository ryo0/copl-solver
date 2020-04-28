package solver

import parser.ast._

object rule {
  sealed class Rule
  case class EInt(value: IntVal) extends Rule
  case class EBool(value: BoolVal) extends Rule
  case class EPlus(e1: Exp,
                   e2: Exp,
                   i3: Exp,
                   e1Rule: Rule,
                   e2Rule: Rule,
                   bPlus: BPlus)
      extends Rule
  case class BPlus(i1: Exp, i2: Exp, i3: Exp) extends Rule
  case class EMinus(e1: Exp,
                    e2: Exp,
                    i3: Exp,
                    e1Rule: Rule,
                    e2Rule: Rule,
                    BMinus: BMinus)
      extends Rule
  case class BMinus(i1: Exp, i2: Exp, i3: Exp) extends Rule
  implicit class RuleValue(rule: Rule) {
    def value(): Exp = {
      rule match {
        case EInt(value)               => value
        case EBool(value)              => value
        case EPlus(_, _, i3, _, _, _)  => i3
        case EMinus(_, _, i3, _, _, _) => i3
      }
    }
  }
}
