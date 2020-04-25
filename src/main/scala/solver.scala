import parser.ast._
import eval.eval._
object solver {

  def initSolve(exp: Exp): String = {
    solve(exp, List())
  }

  def removeEnv(x: String, env: List[(String, Exp)]): List[(String, Exp)] = {
    var removed = false
    var result: List[(String, Exp)] = List()
    for (e <- env) {
      if (removed || e._1 != x) {
        result = result :+ e
      } else {
        removed = true
      }
    }
    result
  }

  def solveFunExp(fun: FunExp,
                  args: List[Exp],
                  env: List[(String, Exp)]): String = {
    if (rmLast(args).isEmpty) {
      solve(fun.body, (fun.param.name, eval(args.head, env)) :: env)
    } else {
      solve(
        FunCall(fun.body, args.tail),
        (fun.param.name, eval(args.head, env)) :: env
      )
    }
  }
  def solve(exp: Exp, env: List[(String, Exp)]): String = {
    exp match {
      case IntVal(n) =>
        envToString(env) + " |- " + s"$n evalto $n by E-Int{};"
      case BoolVal(b) =>
        envToString(env) + " |- " + s"$b evalto $b by E-Bool{};"
      case Var(n) =>
        if (env.head._1 == n) {
          envToString(env) + " |- " + s"$n evalto ${funExpToStringWithEnv(env.head._2, List())} by E-Var1{};"
        } else {
          s"${envToString(env)} |- $n evalto ${funExpToStringWithEnv(eval(exp, env.tail), List())} by E-Var2 {\n" +
            solve(exp, env.tail) + s"};"
        }
      case FunExp(params, body) =>
        val removedEnv = removeEnv(params.name, env)
        s"${envToString(env)} |- ${expToString(FunExp(params, body), env)} evalto " +
          s"${funExpToStringWithEnv(Closure(removedEnv, FunExp(params, body)), List())} by E-Fun{};"
      case FunCall(funName, args) =>
        val funCallE =
          s"${envToString(env)} |- ${expToString(FunCall(funName, args), env)} evalto ${funExpToStringWithEnv(eval(exp, env), env)} by E-App {"
        val cond1 = funName match {
          case FunExp(param, body) =>
            val rmLastArgs = rmLast(args)
            if (rmLastArgs.isEmpty) {
              solve(FunExp(param, body), env)
            } else {
              solve(FunCall(FunExp(param, body), rmLast(args)), env)
            }
          case Var(n) =>
            val rmLastArgs = rmLast(args)
            if (rmLastArgs.isEmpty) {
              solve(Var(n), env)
            } else {
              solve(FunCall(Var(n), rmLastArgs), env)
            }
          case _ =>
            throw new Exception("error: funNameがVarでもFunExpでもない")
        }
        val cond2 = solve(args.last, env)
        val cond3 = funName match {
          case FunExp(param, body) =>
            solveFunExp(FunExp(param, body), args, env)
          case Var(n) =>
            val fun = getValFromEnv(n, env)
            fun match {
              case FunExp(param, body) =>
                solveFunExp(FunExp(param, body), args, List())
              case Closure(e, FunExp(param, body)) =>
                solveFunExp(FunExp(param, body), args, e)
              case _ =>
                throw new Exception("error")
            }
          case _ =>
            throw new Exception("error: funNameがVarでもFunExpでもない")
        }
        s"$funCallE \n $cond1 \n $cond2 \n $cond3 };"
      case LetExp(variable, valueExp, inExp) =>
        val let = envToString(env) + " |- " + s" let " + expToString(
          variable,
          env
        ) + " = "
        val value = eval(valueExp, env)
        val newEnv = (variable.name, value) :: env
        val in = eval(inExp, newEnv)
        val E = let + expToString(valueExp, env) + " in " + expToString(
          inExp,
          env
        ) + " evalto " + funExpToStringWithEnv(in, List()) + " by E-Let {"
        val cond1 = solve(valueExp, env)
        val cond2 = solve(inExp, newEnv)
        s"$E \n $cond1 \n $cond2\n };"
      case IfExp(condExp, thenExp, elseExp) =>
        val start = expToString(exp, env)
        val condVal = eval(condExp, env)
        if (condVal.asInstanceOf[BoolVal].value) {
          val thenVal = eval(thenExp, env)
          val E = envToString(env) + " |- " + s"$start evalto ${funExpToStringWithEnv(thenVal, env)} by E-IfT {\n"
          val cond1 = solve(condExp, env)
          val cond2 = solve(thenExp, env)
          E + cond1 + cond2 + "};"
        } else {
          val elseVal = eval(elseExp, env)
          val E = envToString(env) + " |- " + s"$start evalto ${funExpToStringWithEnv(elseVal, env)} by E-IfF {\n"
          val cond1 = solve(condExp, env)
          val cond2 = solve(elseExp, env)
          E + cond1 + cond2 + "};"
        }
      case InfixExp(left, op, right) =>
        val leftVal = eval(left, env)
        val rightVal = eval(right, env)
        op match {
          case Plus | Minus | Asterisk | LessThan | GreaterThan =>
            val leftValue = leftVal.asInstanceOf[IntVal].value
            val rightValue = rightVal.asInstanceOf[IntVal].value
            op match {
              case Plus =>
                val result = leftValue + rightValue
                val E = envToString(env) + " |- " + s"${expToString(exp, env)} evalto $result by E-Plus{"
                val inBrace = solve(left, env) + "\n" + solve(right, env) + "\n" + s"$leftValue plus $rightValue is $result by B-Plus{};"
                E + "\n" + inBrace + "\n};\n"
              case Minus =>
                if (leftValue == 0) {
                  return envToString(env) + " |- " + s" -$rightValue evalto  -$rightValue by E-Int{};"
                }
                val result = leftValue - rightValue
                val E = envToString(env) + " |- " + s"${expToString(exp, env)} evalto $result by E-Minus{"
                val inBrace = solve(left, env) + "\n" + solve(right, env) + "\n" + s"$leftValue minus $rightValue is $result by B-Minus{};"
                E + "\n" + inBrace + "\n};"
              case Asterisk =>
                val result = leftValue * rightValue
                val E = envToString(env) + " |- " + s"${expToString(exp, env)} evalto $result by E-Times{"
                val inBrace = solve(left, env) + "\n" + solve(right, env) + "\n" + s"$leftValue times $rightValue is $result by B-Times{};"
                E + "\n" + inBrace + "\n};\n"
              case LessThan =>
                val result = leftValue < rightValue
                val E = envToString(env) + " |- " + s"${expToString(exp, env)} evalto $result by E-Lt{"
                val inBrace = solve(left, env) + "\n" + solve(right, env) + "\n" + s"$leftValue less than $rightValue is $result by B-Lt{};"
                E + "\n" + inBrace + "\n};\n"
              case GreaterThan =>
                val result = leftValue > rightValue
                val E = envToString(env) + " |- " + s"${expToString(exp, env)} evalto $result by E-Gt{"
                val inBrace = solve(left, env) + "\n" + solve(right, env) + "\n" + s"$leftValue greater than $rightValue is $result by E-Gt{};"
                E + "\n" + inBrace + "\n};\n"
            }
        }
    }
  }

  val opMap: Map[Op, String] = Map(
    Plus -> "+",
    Minus -> "-",
    Asterisk -> "*",
    LessThan -> "<",
    GreaterThan -> ">"
  )

  def funExpToStringWithEnv(exp: Exp, env: List[(String, Exp)]): String = {
    exp match {
      case FunExp(param, body) =>
        val funStr = s"${expToString(FunExp(param, body), env)}"
        if (funStr.head == '(') {
          s"(${envToString(env)}) [${funStr.slice(1, funStr.length - 1)}]"
        } else {
          s"(${envToString(env)}) [${expToString(FunExp(param, body), env)}]"
        }
      case Closure(e, FunExp(param, body)) =>
        val funStr = s"${expToString(FunExp(param, body), env)}"
        if (funStr.head == '(') {
          s"(${envToString(e)}) [${funStr.slice(1, funStr.length - 1)}]"
        } else {
          s"(${envToString(e)}) [${expToString(FunExp(param, body), env)}]"
        }
      case _ =>
        expToString(exp, env)
    }
  }

  def rmLast(args: List[Exp]): List[Exp] = {
    args.slice(0, args.length - 1)
  }

  def expToString(exp: Exp, env: List[(String, Exp)]): String = {
    exp match {
      case IntVal(n)  => s"$n"
      case BoolVal(b) => s"$b"
      case Var(n)     => s"$n"
      case InfixExp(IntVal(0), Minus, right) =>
        s"-${expToString(right, env)}"
      case InfixExp(left, op, right) =>
        s"(${expToString(left, env)} ${opMap(op)} ${expToString(right, env)})"
      case IfExp(condExp, thenExp, elseExp) =>
        s"if ${expToString(condExp, env)} then ${expToString(thenExp, env)} else ${expToString(elseExp, env)}"
      case LetExp(variable, valueExp, inExp) =>
        s"let ${expToString(variable, env)} = ${expToString(valueExp, env)} in ${expToString(inExp, env)}"
      case FunExp(param, body) =>
        val paramsStr = param.name.mkString
        s"(fun $paramsStr -> ${expToString(body, env)})"
      case Closure(e, FunExp(param, body)) =>
        funExpToStringWithEnv(FunExp(param, body), e ::: env)
      case FunCall(funName, params) =>
        s"(${expToString(funName, env)} ${expsToString(params, env)})"
    }
  }

  def expsToString(params: List[Exp], env: List[(String, Exp)]): String = {
    params.map(p => expToString(p, env) + " ").mkString
  }

  def envToString(env: List[(String, Exp)]): String = {
    if (env.isEmpty) {
      return ""
    }
    val result =
      env
        .map(e => {
          val result = eval(e._2, env)
          result match {
            case Closure(ce, FunExp(p, b)) =>
              s"${e._1} = ${funExpToStringWithEnv(FunExp(p, b), List())},"
            case _ =>
              s"${e._1} = ${funExpToStringWithEnv(result, List())},"
          }
        })
        .reverse
    val resultStr = result.mkString
    resultStr.slice(0, resultStr.length - 1)
  }
}
