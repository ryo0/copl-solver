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
      case IfExp(condExp, thenExp, elseExp) =>
        val start = expToString(exp, env)
        val condVal = eval(condExp, env)
        if (condVal.asInstanceOf[BoolVal].value) {
          val thenVal = eval(thenExp, env)
          val E = envToString(env) + " |- " + s"$start evalto ${expToString(thenVal, env)} by E-IfT {\n"
          val cond1 =  solve(condExp, env)
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
                val E =  envToString(env) + " |- " + s"${expToString(exp, env)} evalto $result by E-Plus{"
                val inBrace = solve(left, env) + "\n" + solve(right, env)  + "\n" + s"$leftValue plus $rightValue is $result by B-Plus{};"
                E + "\n" + inBrace + "\n};\n"
              case Minus =>
                if (leftValue == 0) {
                  return  envToString(env) + " |- " + s" -$rightValue evalto  -$rightValue by E-Int{};"
                }
                val result = leftValue - rightValue
                val E =   envToString(env) + " |- " + s"${expToString(exp, env)} evalto $result by E-Minus{"
                val inBrace = solve(left, env) + "\n" + solve(right, env) + "\n" + s"$leftValue minus $rightValue is $result by B-Minus{};"
                E + "\n" + inBrace + "\n};"
              case Asterisk =>
                val result = leftValue * rightValue
                val E =  envToString(env) + " |- " + s"${expToString(exp, env)} evalto $result by E-Times{"
                val inBrace = solve(left, env) + "\n" + solve(right, env)+ "\n" + s"$leftValue times $rightValue is $result by B-Times{};"
                E + "\n" + inBrace + "\n};\n"
              case LessThan =>
                val result = leftValue < rightValue
                val E = envToString(env) + " |- " + s"${expToString(exp, env)} evalto $result by E-Lt{"
                val inBrace = solve(left, env) + "\n" + solve(right, env) + "\n" + s"$leftValue less than $rightValue is $result by B-Lt{};"
                E + "\n" + inBrace + "\n};\n"
              case GreaterThan =>
                val result = leftValue > rightValue
                val E = envToString(env) + " |- " +  s"${expToString(exp, env)} evalto $result by E-Gt{"
                val inBrace = solve(left, env) + "\n" + solve(right, env) + "\n" + s"$leftValue greater than $rightValue is $result by E-Gt{};"
                E + "\n" + inBrace + "\n};\n"
            }
        }
    }
  }

  val opMap: Map[Op, String] = Map(Plus -> "+", Minus -> "-", Asterisk -> "*", LessThan -> "<", GreaterThan -> ">")
  def expToString(exp: Exp, env: List[(String, Exp)]): String = {
    exp match {
      case IntVal(n) =>  s"$n"
      case InfixExp(IntVal(0), Minus, right) =>
        s"-${expToString(right, env)}"
      case InfixExp(left, op, right) =>
        s"(${expToString(left, env)} ${opMap(op)} ${expToString(right, env)})"
      case IfExp(condExp, thenExp, elseExp) => {
        s"if ${expToString(condExp, env)} then ${expToString(thenExp, env)} else ${expToString(elseExp, env)}"
      }
    }
  }
  def envToString(env: List[(String, Exp)]): String = {
    val result = env.map(e =>
      s"${e._1} = ${eval(e._2, env)},"
    )
    result.slice(0, result.length - 1).mkString
  }
}