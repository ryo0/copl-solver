package eval
import parser.ast._

object eval {
  def initEval(exp: Exp): Exp = {
    eval(exp, List())
  }

  def getValFromEnv(key: String, env: List[(String, Exp)]): Exp = {
    for (e <- env) {
      if (e._1 == key) {
        return e._2
      }
    }
    throw new Exception("keyがenvにありません")
  }

  def eval(exp: Exp, env: List[(String, Exp)]): Exp = {
    exp match {
      case IntVal(n)  => IntVal(n)
      case Var(n)     => getValFromEnv(n, env)
      case BoolVal(b) => BoolVal(b)
      case LetExp(variable, valueExp, inExp) =>
        val newEnv = (variable.name, eval(valueExp, env))
        eval(inExp, newEnv :: env)
      case LetRecExp(variable, fun, inExp) =>
        val evaledFun = eval(fun, (variable.name, eval(fun, env)) :: env)
        val newEnv = (variable.name, evaledFun)
        eval(inExp, newEnv :: env)
      case IfExp(condExp, thenExp, elseExp) => {
        if (eval(condExp, env).asInstanceOf[BoolVal].value) {
          eval(thenExp, env)
        } else {
          eval(elseExp, env)
        }
      }
      case FunExp(param, body) =>
        Closure(env, FunExp(param, body))
      case RecFunExp(variable, param, body) =>
        RecClosure(env, RecFunExp(variable, param, body))
      case Closure(e, FunExp(param, body)) =>
        Closure(e, FunExp(param, body))
      case RecClosure(e, RecFunExp(variable, param, body)) =>
        RecClosure(e, RecFunExp(variable, param, body))
      case FunCall(exp, arg) =>
        val evaledExp = eval(exp, env)
        evaledExp match {
          case FunExp(p, b) =>
            eval(b, (p.name, eval(arg, env)) :: env)
          case RecFunExp(v, p, b) =>
            eval(b, (p.name, eval(arg, env)) :: env)
          case Closure(e, FunExp(p, b)) =>
            eval(b, (p.name, eval(arg, env)) :: e ::: env)
          case RecClosure(e, RecFunExp(v, p, b)) =>
            eval(b, (p.name, eval(arg, env)) :: e ::: env)
          case _ =>
            throw new Exception("error")
        }
      case InfixExp(left, op, right) =>
        val leftVal = eval(left, env)
        val rightVal = eval(right, env)
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
            BoolVal(
              leftVal.asInstanceOf[IntVal].value < rightVal
                .asInstanceOf[IntVal]
                .value
            )
          case GreaterThan =>
            BoolVal(
              leftVal.asInstanceOf[IntVal].value > rightVal
                .asInstanceOf[IntVal]
                .value
            )
        }
    }
  }

  def makeNewEnv(param: Var,
                 arg: Exp,
                 env: List[(String, Exp)]): List[(String, Exp)] = {
    (param.name, eval(arg, env)) :: env
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
