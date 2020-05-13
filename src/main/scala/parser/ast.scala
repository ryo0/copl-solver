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
          EVar(env, Var(n))
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
        case EmptyList =>
          ENil(env, EmptyList)
        case EList(e1, e2) =>
          val r1 = e1.solve(env)
          val r2 = e2.solve(env)
          ECons(env, e1, e2, r1, r2)
        case Match(e0, Pattern(p, e) :: List()) =>
          val r1 = e0.solve(env)
          val v = r1.value
          val mr = p.matches(v)
          val r2 = e.solve(mr.getEnv.append(env))
          EMatchM1(env, e0, p, e, r1, mr, r2)
        case Match(e0, Pattern(p, e) :: c) =>
          val r1 = e0.solve(env)
          val v = r1.value
          if (p.checkMatching(v)) {
            val mr = p.matches(v)
            val r2 = e.solve(mr.getEnv.append(env))
            EMatchM2(env, e0, p, e, c, r1, mr, r2)
          } else {
            val nmr = p.notMatch(v)
            val r2 = Match(e0, c).solve(env)
            EMatchN(env, e0, p, e, c, r1, nmr, r2)
          }
        case _ =>
          throw new Exception("未対応")
      }
    }
    def checkMatching(v: Exp): Boolean = {
      (this, v) match {
        case (EmptyList, EmptyList) =>
          true
        case (WildCard, _) =>
          true
        case (EList(p1, p2), EList(v1, v2)) =>
          p1.checkMatching(v1) && p2.checkMatching(v2)
        case (EmptyList, EList(v1, v2)) =>
          false
        case (EList(p1, p2), EmptyList) =>
          false
        case (x, v) =>
          true
      }
    }
    def matches(v: Exp): MatchRule = {
      (this, v) match {
        case (EmptyList, EmptyList) =>
          MNil(List(), EmptyList, EmptyList)
        case (WildCard, _) =>
          MWild(List(), v)
        case (EList(p1, p2), EList(v1, v2)) =>
          val mr1 = p1.matches(v1)
          val mr2 = p2.matches(v2)
          MCons(mr2.getEnv.appendNoDouble(mr1.getEnv), p1, p2, v1, v2, mr1, mr2)
        case (x, v) =>
          println(x)
          println(v)
          val env: Env = List((x.string, v))
          MVar(env, x, v)
      }
    }
    def notMatch(v: Exp): NotMatchRule = {
      (this, v) match {
        case (EmptyList, EList(v1, v2)) =>
          NMConsNil(v1, v2)
        case (EList(p1, p2), EmptyList) =>
          NMNilCons(p1, p2)
        case (EList(p1, p2), EList(v1, v2)) =>
          if (!p1.checkMatching(v1)) {
            println("this, v")
            println(this, v)
            println("p, v")
            println(p1, v1)
            val nmr = p1.notMatch(v1)
            NMConsConsL(p1, p2, v1, v2, nmr)
          } else if (!p2.checkMatching(v2)) {
            val nmr = p2.notMatch(v2)
            NMConsConsR(p1, p2, v1, v2, nmr)
          } else {
            throw new Exception("not Matchが誤って呼ばれている")
          }
      }
    }
    def string: String = {
      this match {
        case IntVal(n)  => s"$n"
        case BoolVal(b) => s"$b"
        case Var(n)     => s"$n"
        case WildCard   => "_"
        case InfixExp(IntVal(0), Minus, right) =>
          s"-${right.string}"
        case InfixExp(left, op, right) =>
          s"(${left.string} ${opMap(op)} ${right.string})"
        case IfExp(condExp, thenExp, elseExp) =>
          s"if ${condExp.string} then ${thenExp.string} else ${elseExp.string}"
        case LetExp(variable, valueExp, inExp) =>
          s"let ${variable.string} = ${valueExp.string} in ${inExp.string}"
        case LetRecExp(variable, fun, inExp) =>
          s"let rec ${variable.string} = ${FunExp(fun.param, fun.body).string
            .dropRight(1)
            .drop(1)} in ${inExp.string}"
        case FunExp(param, body) =>
          val paramsStr = param.name.mkString
          s"(fun $paramsStr -> ${body.string})"
        case RecFunExp(v, param, body) =>
          val paramsStr = param.name.mkString
          s"(fun $paramsStr -> ${body.string})"
        case Closure(e, FunExp(param, body)) =>
          val funString = FunExp(param, body).string
          s"(${e.string}) [${funString.dropRight(1).drop(1)}]"
        case RecClosure(e, RecFunExp(v, param, body)) =>
          val funString = FunExp(param, body).string
          s"(${e.string}) [rec ${v.string} = ${funString.dropRight(1).drop(1)}]"
        case FunCall(funName, arg) =>
          s"(${funName.string} ${arg.string})"
        case EList(left, right) =>
          if (right.isInstanceOf[ListExp]) {
            s"(${left.string} :: ${right.string})"
          } else {
            s"${left.string} :: ${right.string}"
          }
        case EmptyList =>
          s"[]"
        case Pattern(left, right) =>
          s"| ${left.string} -> ${right.string} "
        case Match(v, patterns) =>
          s"match ${v.string} with " + patterns
            .map(p => p.string)
            .mkString
            .drop(1)
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
  case class Pattern(left: ListExp, right: Exp) extends Exp
  case class Match(v: Var, patterns: List[Pattern]) extends Exp
  object WildCard extends Exp
}
