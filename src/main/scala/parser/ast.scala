package parser

import solver.rule._

object ast {
  sealed class Op
  object Plus extends Op
  object Minus extends Op
  object Asterisk extends Op
  object Slash extends Op
  object GreaterThan extends Op
  object LessThan extends Op
  object Cons extends Op

  sealed class Exp {
    def solve(env: Env): Rule = {
      this match {
        case IntVal(n) =>
          EInt(env, IntVal(n))
        case BoolVal(b) =>
          EBool(env, BoolVal(b))
        case Var(n) =>
          if (env.head._1 == n) {
            EVar1(env, Var(n))
          } else {
            EVar2(env, Var(n), Var(n).solve(env.tail))
          }
        case InfixExp(e1, Plus, e2) =>
          val r1 = e1.solve(env)
          val r2 = e2.solve(env)
          val i3 = r1.value.asInstanceOf[IntVal] + r2.value
            .asInstanceOf[IntVal]
          EPlus(env, e1, e2, i3, r1, r2, BPlus(r1.value, r2.value, i3))
        case InfixExp(IntVal(0), Minus, e2) =>
          val r2 = e2.solve(env)
          val i3 = IntVal(
            -1 * r2.value
              .asInstanceOf[IntVal]
              .value
          )
          i3.solve(env)
        case InfixExp(e1, Minus, e2) =>
          val r1 = e1.solve(env)
          val r2 = e2.solve(env)
          val i3 = r1.value.asInstanceOf[IntVal] - r2.value
            .asInstanceOf[IntVal]
          EMinus(env, e1, e2, i3, r1, r2, BMinus(r1.value, r2.value, i3))
        case InfixExp(e1, Asterisk, e2) =>
          val r1 = e1.solve(env)
          val r2 = e2.solve(env)
          val i3 = r1.value.asInstanceOf[IntVal] * r2.value
            .asInstanceOf[IntVal]
          ETimes(env, e1, e2, i3, r1, r2, BTimes(r1.value, r2.value, i3))
        case InfixExp(e1, LessThan, e2) =>
          val r1 = e1.solve(env)
          val r2 = e2.solve(env)
          val i3 = r1.value.asInstanceOf[IntVal] < r2.value
            .asInstanceOf[IntVal]
          ELt(env, e1, e2, i3, r1, r2, BLt(r1.value, r2.value, i3))
        case IfExp(condExp, thenExp, elseExp) =>
          val r1 = condExp.solve(env)
          if (r1.value.asInstanceOf[BoolVal].value) {
            val r2 = thenExp.solve(env)
            EIfT(env, condExp, thenExp, elseExp, r1, r2)
          } else {
            val r3 = elseExp.solve(env)
            EIfF(env, condExp, thenExp, elseExp, r1, r3)
          }
        case LetExp(variable, valueExp, inExp) =>
          val r1 = valueExp.solve(env)
          val r2 = inExp.solve((variable.name, r1.value) :: env)
          ELet(env, variable, valueExp, inExp, r1, r2)
        case LetRecExp(variable, fun, inExp) =>
          val newEnv = (variable.name, fun.solve(env).value) :: env
          val in = inExp.solve(newEnv)
          ELetRec(env, variable, fun, inExp, in)
        case FunExp(variable: Var, body: Exp) =>
          EFun(env, variable, body, EClosure(env, variable, body))
        case RecFunExp(variable: Var, param: Var, body: Exp) =>
          ERecFun(
            env,
            variable,
            param,
            body,
            ERecClosure(env, variable, param, body)
          )
        case FunCall(funName: Exp, arg: Exp) =>
          funName match {
            case RecFunExp(v, p, b) =>
              val r1 = funName.solve(env)
              val recClosure = r1.value.asInstanceOf[RecClosure]
              val r2 = arg.solve(env)
              val r3 = recClosure.recFunExp.body.solve(
                (recClosure.recFunExp.param.name, r2.value) ::
                  (recClosure.recFunExp.variable.name, recClosure)
                  :: recClosure.env
              )
              EAppRec(env, funName, arg, r1, r2, r3)
            case RecClosure(ce, RecFunExp(v, p, b)) =>
              val r1 = funName.solve(ce)
              val recClosure = r1.value.asInstanceOf[RecClosure]
              val r2 = arg.solve(ce)
              val r3 = recClosure.recFunExp.body.solve(
                (recClosure.recFunExp.param.name, r2.value) ::
                  (recClosure.recFunExp.variable.name, recClosure)
                  :: recClosure.env
              )
              EAppRec(ce ::: env, funName, arg, r1, r2, r3)
            case _ =>
              val r1 = funName.solve(env)
              r1.value match {
                case RecClosure(ce, RecFunExp(v, p, b)) =>
                  val r2 = arg.solve(env)
                  val r3 =
                    b.solve((p.name, r2.value) :: (v.name, r1.value) :: ce)
                  EAppRec(ce ::: env, funName, arg, r1, r2, r3)
                case Closure(ce, FunExp(p, b)) =>
                  val r2 = arg.solve(env)
                  val r3 = b.solve((p.name, r2.value) :: ce)
                  EApp(env, funName, arg, r1, r2, r3)
              }
          }

        case _ =>
          throw new Exception("未対応")
      }
    }
  }
  case class IntVal(value: Int) extends Exp {
    def +(other: IntVal): IntVal = {
      IntVal(value + other.value)
    }
    def -(other: IntVal): IntVal = {
      IntVal(value - other.value)
    }
    def *(other: IntVal): IntVal = {
      IntVal(value * other.value)
    }
    def <(other: IntVal): BoolVal = {
      BoolVal(value < other.value)
    }
  }
  case class Var(name: String) extends Exp
  case class BoolVal(value: Boolean) extends Exp
  case class InfixExp(leftExp: Exp, op: Op, rightExp: Exp) extends Exp
  case class IfExp(condExp: Exp, thenExp: Exp, elseExp: Exp) extends Exp
  case class LetExp(variable: Var, valueExp: Exp, inExp: Exp) extends Exp
  case class FunExp(param: Var, body: Exp) extends Exp
  case class FunCall(funName: Exp, arg: Exp) extends Exp
  case class Closure(env: List[(String, Exp)], funExp: FunExp) extends Exp
  case class RecFunExp(variable: Var, param: Var, body: Exp) extends Exp
  case class RecClosure(env: List[(String, Exp)], recFunExp: RecFunExp)
      extends Exp
  case class LetRecExp(variable: Var, valueExp: RecFunExp, inExp: Exp)
      extends Exp
  sealed class ListExp extends Exp
  case class EList(first: Exp, second: Exp) extends ListExp
  object EmptyList extends ListExp
}
