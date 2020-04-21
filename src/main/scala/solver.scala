import parser.ast._
import eval.eval.eval

object solver {

  def initSolve(exp: Exp): String = {
    solve(exp, List())
  }

  def solve(exp: Exp, env: List[(String, Exp)]): String = {
    exp match {
      case IntVal(n) =>
        envToString(env) + " |- " + s"$n evalto $n by E-Int{};"
      case BoolVal(b) =>
        envToString(env) + " |- " + s"$b evalto $b by E-Bool{};"
      case Var(n) =>
        if (env.head._1 == n) {
          envToString(env) + " |- " + s"$n evalto ${expToString(env.head._2, env)} by E-Var1{};"
        } else {
          s"${envToString(env)} |- $n evalto ${expToString(eval(exp, env.tail), env.tail)} by E-Var2 {" +
            solve(exp, env.tail) + s"};"
        }
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
        ) + " evalto " + expToString(in, newEnv) + " by E-Let {"
        val cond1 = solve(valueExp, env)
        val cond2 = solve(inExp, (variable.name, value) :: env)
        s"$E \n $cond1 \n $cond2\n };"
      case IfExp(condExp, thenExp, elseExp) =>
        val start = expToString(exp, env)
        val condVal = eval(condExp, env)
        if (condVal.asInstanceOf[BoolVal].value) {
          val thenVal = eval(thenExp, env)
          val E = envToString(env) + " |- " + s"$start evalto ${expToString(thenVal, env)} by E-IfT {\n"
          val cond1 = solve(condExp, env)
          val cond2 = solve(thenExp, env)
          E + cond1 + cond2 + "};"
        } else {
          val elseVal = eval(elseExp, env)
          val E = envToString(env) + " |- " + s"$start evalto ${expToString(elseVal, env)} by E-IfF {\n"
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
      case FunExp(params, body) =>
        val paramsStr = params.map(p => p.name + " ").mkString
        s"fun $paramsStr -> ${expToString(body, env)}"
      case FunCall(funName, params) =>
        s"(${expToString(funName, env)} ${expsToString(params, env)})"
    }
  }

  def expsToString(params: List[Exp], env: List[(String, Exp)]): String = {
    params.map(p => expToString(p, env) + " ").mkString
  }

  def envToString(env: List[(String, Exp)]): String = {
    val result =
      env.map(e => s"${e._1} = ${expToString(eval(e._2, env), env)},").reverse
    val resultStr = result.mkString
    resultStr.slice(0, resultStr.length - 1)
  }
}
