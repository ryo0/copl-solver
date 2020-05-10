package solver

import parser.ast._

object rule {
  type Env = List[(String, Exp)]
  implicit class environment(env: Env) {
    def string: String = {
      env.reverse.map(e => s"${e._1} = ${e._2.string},").mkString.dropRight(1)
    }
  }
  val opMap: Map[Op, String] = Map(
    Plus -> "+",
    Minus -> "-",
    Asterisk -> "*",
    LessThan -> "<",
    GreaterThan -> ">"
  )

  sealed class Rule
  case class EInt(env: Env, value: IntVal) extends Rule
  case class EBool(env: Env, value: BoolVal) extends Rule
  case class EPlus(env: Env,
                   e1: Exp,
                   e2: Exp,
                   i3: Exp,
                   e1Rule: Rule,
                   e2Rule: Rule,
                   bPlus: BPlus)
      extends Rule
  case class BPlus(i1: Exp, i2: Exp, i3: Exp) extends Rule
  case class EMinus(env: Env,
                    e1: Exp,
                    e2: Exp,
                    i3: Exp,
                    e1Rule: Rule,
                    e2Rule: Rule,
                    BMinus: BMinus)
      extends Rule
  case class BMinus(i1: Exp, i2: Exp, i3: Exp) extends Rule
  case class ETimes(env: Env,
                    e1: Exp,
                    e2: Exp,
                    i3: Exp,
                    e1Rule: Rule,
                    e2Rule: Rule,
                    bMull: BTimes)
      extends Rule
  case class BTimes(i1: Exp, i2: Exp, i3: Exp) extends Rule

  case class ELt(env: Env,
                 e1: Exp,
                 e2: Exp,
                 i3: Exp,
                 e1Rule: Rule,
                 e2Rule: Rule,
                 bLt: BLt)
      extends Rule
  case class BLt(i1: Exp, i2: Exp, i3: Exp) extends Rule
  case class EIfT(env: Env,
                  e1: Exp,
                  e2: Exp,
                  e3: Exp,
                  e1Rule: Rule,
                  e2Rule: Rule)
      extends Rule
  case class EIfF(env: Env,
                  e1: Exp,
                  e2: Exp,
                  e3: Exp,
                  e1Rule: Rule,
                  e3Rule: Rule)
      extends Rule
  case class EVar1(env: Env, variable: Var) extends Rule
  case class EVar2(env: Env, variable: Var, r: Rule) extends Rule
  case class ELet(env: Env, variable: Exp, e1: Exp, e2: Exp, r1: Rule, r2: Rule)
      extends Rule
  case class ELetRec(env: Env, variable: Exp, e1: Exp, e2: Exp, r: Rule)
      extends Rule
  case class EFun(env: Env, param: Var, e: Exp, closure: EClosure) extends Rule
  case class ERecFun(env: Env,
                     variable: Var,
                     param: Var,
                     e: Exp,
                     closure: ERecClosure)
      extends Rule
  case class EClosure(env: Env, param: Var, e: Exp) extends Rule
  case class ERecClosure(env: Env, variable: Var, param: Var, e: Exp)
      extends Rule
  case class EApp(env: Env, e1: Exp, e2: Exp, r1: Rule, r2: Rule, r3: Rule)
      extends Rule
  case class EAppRec(env: Env, e1: Exp, e2: Exp, r1: Rule, r2: Rule, r3: Rule)
      extends Rule
  case class ENil(env: Env, emptyList: ListExp) extends Rule
  case class ECons(env: Env, e1: Exp, e2: Exp, r1: Rule, r2: Rule) extends Rule
  case class EMatchNil(env: Env,
                       e1: Exp,
                       e2: Exp,
                       e3: Exp,
                       x: Exp,
                       y: Exp,
                       r1: Rule,
                       r2: Rule)
      extends Rule
  case class EMatchCons(env: Env,
                        e1: Exp,
                        e2: Exp,
                        e3: Exp,
                        x: Exp,
                        y: Exp,
                        r1: Rule,
                        r2: Rule)
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
    def value: Exp = {
      rule match {
        case EInt(_, value)                   => value
        case EBool(_, value)                  => value
        case EPlus(_, _, _, i3, _, _, _)      => i3
        case EMinus(_, _, _, i3, _, _, _)     => i3
        case ETimes(_, _, _, i3, _, _, _)     => i3
        case ELt(_, _, _, i3, _, _, _)        => i3
        case EIfT(_, _, _, _, _, e2Rule)      => e2Rule.value
        case EIfF(_, _, _, _, _, e3Rule)      => e3Rule.value
        case EVar1(env, _)                    => env.head._2
        case EVar2(_, _, rule)                => rule.value
        case ELet(_, _, _, _, _, r2)          => r2.value
        case ELetRec(_, _, _, _, r)           => r.value
        case EFun(_, _, _, eClosure)          => eClosure.value
        case ERecFun(_, _, _, _, eRecClosure) => eRecClosure.value
        case EClosure(env, variable, body) =>
          Closure(env, FunExp(variable, body))
        case ERecClosure(env, variable: Var, param: Var, body) =>
          RecClosure(env, RecFunExp(variable, param, body))
        case EApp(_, _, _, _, _, r3)             => r3.value
        case EAppRec(_, _, _, _, _, r3)          => r3.value
        case ENil(_, emp)                        => emp
        case ECons(_, _, _, r1, r2)              => EList(r1.value, r2.value)
        case EMatchNil(_, _, _, _, _, _, _, r2)  => r2.value
        case EMatchCons(_, _, _, _, _, _, _, r2) => r2.value

      }
    }

    def string(nest: Int = 0): String = {
      val indent = "     ".mul(nest)
      val indentPlus1 = "     ".mul(nest + 1)
      rule match {
        case EInt(env, value) =>
          s"${env.string} |- ${value.string} evalto ${value.string} by E-Int{};"
        case EBool(env, value) =>
          s"${env.toString()} |- ${value.string} evalto ${value.string} by E-Bool{};"
        case EVar1(env, variable) =>
          s"${env.string} |- ${variable.name} evalto ${env.head._2.string} by E-Var1{};"
        case EVar2(env, variable, rule) =>
          s"${env.string} |- ${variable.name} evalto ${rule.value.string} by E-Var2{\n" +
            s"$indentPlus1${rule.string(nest + 1)}\n" +
            s"$indent};"
        case EPlus(env, e1, e2, i3, e1Rule, e2Rule, bPlus) =>
          s"${env.string} |- ${e1.string} + ${e2.string} evalto ${i3.string} by E-Plus{\n" +
            s"$indentPlus1${e1Rule.string(nest + 1)}\n" +
            s"$indentPlus1${e2Rule.string(nest + 1)}\n" +
            s"$indentPlus1${bPlus.string(nest + 1)}\n" +
            s"$indent};"
        case BPlus(i1, i2, i3) =>
          s"${i1.string} plus ${i2.string} is ${i3.string} by B-Plus{};"
        case EMinus(env, e1, e2, i3, e1Rule, e2Rule, bMinus) =>
          s"${env.string} |- ${e1.string} - ${e2.string} evalto ${i3.string} by E-Minus{\n" +
            s"$indentPlus1${e1Rule.string(nest + 1)}\n" +
            s"$indentPlus1${e2Rule.string(nest + 1)}\n" +
            s"$indentPlus1${bMinus.string(nest + 1)}\n" +
            s"$indent};"
        case EMinus(env, IntVal(0), e2, i3, e1Rule, e2Rule, bMinus) =>
          s"${env.string} - ${e2.string} evalto ${i3.string} by E-Minus{\n" +
            s"$indentPlus1${e1Rule.string(nest + 1)}\n" +
            s"$indentPlus1${e2Rule.string(nest + 1)}\n" +
            s"$indentPlus1${bMinus.string(nest + 1)}\n" +
            s"$indent};"
        case BMinus(i1, i2, i3) =>
          s"${i1.string} minus ${i2.string} is ${i3.string} by B-Minus{};"
        case ETimes(env, e1, e2, i3, e1Rule, e2Rule, bMinus) =>
          s"${env.string} |- ${e1.string} * ${e2.string} evalto ${i3.string} by E-Times{\n" +
            s"$indentPlus1${e1Rule.string(nest + 1)}\n" +
            s"$indentPlus1${e2Rule.string(nest + 1)}\n" +
            s"$indentPlus1${bMinus.string(nest + 1)}\n" +
            s"$indent};"
        case BTimes(i1, i2, i3) =>
          s"${i1.string} times ${i2.string} is ${i3.string} by B-Times{};"
        case ELt(env, e1, e2, i3, e1Rule, e2Rule, bLt) =>
          s"${env.string} |- ${e1.string} < ${e2.string} evalto ${i3.string} by E-Lt{\n" +
            s"$indentPlus1${e1Rule.string(nest + 1)}\n" +
            s"$indentPlus1${e2Rule.string(nest + 1)}\n" +
            s"$indentPlus1${bLt.string(nest + 1)}\n" +
            s"$indent};"
        case BLt(i1, i2, i3) =>
          s"${i1.string} less than ${i2.string} is ${i3.string} by B-Lt{};"
        case EIfT(env, e1, e2, e3, e1Rule, e2Rule) =>
          s"${env.string} |- if ${e1.string} then ${e2.string} else ${e3.string} evalto ${e2Rule.value.string} by E-IfT{\n" +
            s"$indentPlus1${e1Rule.string(nest + 1)}\n" +
            s"$indentPlus1${e2Rule.string(nest + 1)}\n" +
            s"$indent};"
        case EIfF(env, e1, e2, e3, e1Rule, e3Rule) =>
          s"${env.string} |- if ${e1.string} then ${e2.string} else ${e3.string} evalto ${e3Rule.value.string} by E-IfF{\n" +
            s"$indentPlus1${e1Rule.string(nest + 1)}\n" +
            s"$indentPlus1${e3Rule.string(nest + 1)}\n" +
            s"$indent};"
        case ELet(env, x, e1, e2, r1, r2) =>
          s"${env.string} |- let ${x.string} = ${e1.string} in ${e2.string} evalto ${r2.value.string} by E-Let{\n" +
            s"$indentPlus1${r1.string(nest + 1)}\n" +
            s"$indentPlus1${r2.string(nest + 1)}\n" +
            s"$indent};"
        case EFun(env, variable, e, closure) =>
          s"${env.string} |- fun ${variable.string} -> ${e.string} evalto ${closure
            .string(nest)} by E-Fun{};"
        case EClosure(env, param, e) =>
          s"(${env.string}) [fun ${param.string} -> ${e.string}]"
        case ERecClosure(env, variable, param, e) =>
          s"(${env.string}) [rec $variable = fun ${param.string} -> ${e.string}]"
        case ELetRec(env, variable, e1, e2, r) =>
          s"${env.string} |- let rec ${variable.string} = ${e1.string
            .dropRight(1)
            .drop(1)} in ${e2.string} evalto ${r.value.string} by E-LetRec{\n" +
            s"$indentPlus1${r.string(nest + 1)}\n" +
            s"$indent};"
        case EApp(env, e1, e2, r1, r2, r3) =>
          s"${env.string} |- ${e1.string} ${e2.string} evalto ${r3.value.string} by E-App{\n" +
            s"$indentPlus1${r1.string(nest + 1)}\n" +
            s"$indentPlus1${r2.string(nest + 1)}\n" +
            s"$indentPlus1${r3.string(nest + 1)}\n" +
            s"$indent};"
        case EAppRec(env, e1, e2, r1, r2, r3) =>
          s"${env.string} |- ${e1.string} ${e2.string} evalto ${r3.value.string} by E-AppRec{\n" +
            s"$indentPlus1${r1.string(nest + 1)}\n" +
            s"$indentPlus1${r2.string(nest + 1)}\n" +
            s"$indentPlus1${r3.string(nest + 1)}\n" +
            s"$indent};"
      }
    }
  }
}
