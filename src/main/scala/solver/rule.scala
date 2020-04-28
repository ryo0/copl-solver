package solver

import parser.ast._

object rule {
  val opMap: Map[Op, String] = Map(
    Plus -> "+",
    Minus -> "-",
    Asterisk -> "*",
    LessThan -> "<",
    GreaterThan -> ">"
  )
  def expToString(exp: Exp): String = {
    exp match {
      case IntVal(n)  => s"$n"
      case BoolVal(b) => s"$b"
      case Var(n)     => s"$n"
      case InfixExp(IntVal(0), Minus, right) =>
        s"-${expToString(right)}"
      case InfixExp(left, op, right) =>
        s"(${expToString(left)} ${opMap(op)} ${expToString(right)})"
      case IfExp(condExp, thenExp, elseExp) =>
        s"if ${expToString(condExp)} then ${expToString(thenExp)} else ${expToString(elseExp)}"
    }
  }
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
  case class ETimes(e1: Exp,
                    e2: Exp,
                    i3: Exp,
                    e1Rule: Rule,
                    e2Rule: Rule,
                    bMull: BTimes)
      extends Rule
  case class BTimes(i1: Exp, i2: Exp, i3: Exp) extends Rule

  case class ELt(e1: Exp,
                 e2: Exp,
                 i3: Exp,
                 e1Rule: Rule,
                 e2Rule: Rule,
                 bLt: BLt)
      extends Rule
  case class BLt(i1: Exp, i2: Exp, i3: Exp) extends Rule
  case class EIfT(e1: Exp, e2: Exp, e3: Exp, e1Rule: Rule, e2Rule: Rule)
      extends Rule
  case class EIfF(e1: Exp, e2: Exp, e3: Exp, e1Rule: Rule, e3Rule: Rule)
      extends Rule

  implicit class NestString(str: String) {
    def mul(nest: Int): String = {
      if (nest == 0) { "" } else if (nest == 1) {
        str
      } else {
        str + str.mul(nest - 1)
      }
    }
  }
  implicit class RuleValue(rule: Rule) {
    def value(): Exp = {
      rule match {
        case EInt(value)               => value
        case EBool(value)              => value
        case EPlus(_, _, i3, _, _, _)  => i3
        case EMinus(_, _, i3, _, _, _) => i3
        case ETimes(_, _, i3, _, _, _) => i3
        case ELt(_, _, i3, _, _, _)    => i3
        case EIfT(_, _, _, _, e2Rule)  => e2Rule.value()
        case EIfF(_, _, _, _, e3Rule)  => e3Rule.value()

      }
    }
    def string(nest: Int = 0): String = {
      val indent = "     ".mul(nest)
      val indentP1 = "     ".mul(nest + 1)
      rule match {
        case EInt(value) =>
          s"${expToString(value)} evalto ${expToString(value)} by E-Int{};"
        case EBool(value) =>
          s"${expToString(value)} evalto ${expToString(value)} by E-Bool{};"
        case EPlus(e1, e2, i3, e1Rule, e2Rule, bPlus) =>
          s"${expToString(e1)} + ${expToString(e2)} evalto ${expToString(i3)} by E-Plus{\n" +
            s"$indentP1${e1Rule.string(nest + 1)}\n" +
            s"$indentP1${e2Rule.string(nest + 1)}\n" +
            s"$indentP1${bPlus.string(nest + 1)}\n" +
            s"$indent};"
        case BPlus(i1, i2, i3) =>
          s"${expToString(i1)} plus ${expToString(i2)} is ${expToString(i3)} by B-Plus{};"
        case EMinus(e1, e2, i3, e1Rule, e2Rule, bMinus) =>
          s"${expToString(e1)} - ${expToString(e2)} evalto ${expToString(i3)} by E-Minus{\n" +
            s"$indentP1${e1Rule.string(nest + 1)}\n" +
            s"$indentP1${e2Rule.string(nest + 1)}\n" +
            s"$indentP1${bMinus.string(nest + 1)}\n" +
            s"$indent};"
        case BMinus(i1, i2, i3) =>
          s"${expToString(i1)} minus ${expToString(i2)} is ${expToString(i3)} by B-Minus{};"
        case ETimes(e1, e2, i3, e1Rule, e2Rule, bMinus) =>
          s"${expToString(e1)} * ${expToString(e2)} evalto ${expToString(i3)} by E-Times{\n" +
            s"$indentP1${e1Rule.string(nest + 1)}\n" +
            s"$indentP1${e2Rule.string(nest + 1)}\n" +
            s"$indentP1${bMinus.string(nest + 1)}\n" +
            s"$indent};"
        case BTimes(i1, i2, i3) =>
          s"${expToString(i1)} times ${expToString(i2)} is ${expToString(i3)} by B-Times{};"
        case ELt(e1, e2, i3, e1Rule, e2Rule, bLt) =>
          s"${expToString(e1)} < ${expToString(e2)} evalto ${expToString(i3)} by E-Lt{\n" +
            s"$indentP1${e1Rule.string(nest + 1)}\n" +
            s"$indentP1${e2Rule.string(nest + 1)}\n" +
            s"$indentP1${bLt.string(nest + 1)}\n" +
            s"$indent};"
        case BLt(i1, i2, i3) =>
          s"${expToString(i1)} less than ${expToString(i2)} is ${expToString(i3)} by B-Lt{};"
        case EIfT(e1, e2, e3, e1Rule, e2Rule) =>
          s"if ${expToString(e1)} then ${expToString(e2)} else ${expToString(e3)} evalto ${expToString(e2Rule.value())} by E-IfT{\n" +
            s"$indentP1${e1Rule.string(nest + 1)}\n" +
            s"$indentP1${e2Rule.string(nest + 1)}\n" +
            s"$indent};"
        case EIfF(e1, e2, e3, e1Rule, e3Rule) =>
          s"if ${expToString(e1)} then ${expToString(e2)} else ${expToString(e3)} evalto ${expToString(e3Rule.value())} by E-IfF{\n" +
            s"$indentP1${e1Rule.string(nest + 1)}\n" +
            s"$indentP1${e3Rule.string(nest + 1)}\n" +
            s"$indent};"
      }
    }
  }
}
