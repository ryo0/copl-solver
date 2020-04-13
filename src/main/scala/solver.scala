import parser.ast._
import eval.eval.eval

object solver {

  def solve(exp: Exp): String = {
    exp match {
      case IntVal(n) =>
        s"$n evalto $n by E-Int{};"
      case BoolVal(b) =>
        s"$b evalto $b by E-Bool{};"
      case IfExp(condExp, thenExp, elseExp) =>
        val start = expToString(exp)
        val condVal = eval(condExp)
        if (condVal.asInstanceOf[BoolVal].value) {
          val thenVal = eval(thenExp)
          val E = s"$start evalto ${expToString(thenVal)} by E-IfT {\n"
          val cond1 = solve(condExp)
          val cond2 = solve(thenExp)
          E + cond1 + cond2 + "};"
        } else {
          val elseVal = eval(elseExp)
          val E = s"$start evalto ${expToString(elseVal)} by E-IfF {\n"
          val cond1 = solve(condExp)
          val cond2 = solve(elseExp)
          E + cond1 + cond2 + "};"
        }
      case InfixExp(left, op, right) =>
        val leftVal = eval(left)
        val rightVal = eval(right)
        op match {
          case Plus | Minus | Asterisk | LessThan | GreaterThan =>
            val leftValue = leftVal.asInstanceOf[IntVal].value
            val rightValue = rightVal.asInstanceOf[IntVal].value
            op match {
              case Plus =>
                val result = leftValue + rightValue
                val E = s"${expToString(exp)} evalto $result by E-Plus{"
                val inBrace = solve(left) + "\n" + solve(right) + "\n" + s"$leftValue plus $rightValue is $result by B-Plus{};"
                E + "\n" + inBrace + "\n};\n"
              case Minus =>
                if (leftValue == 0) {
                  return s" -$rightValue evalto  -$rightValue by E-Int{};"
                }
                val result = leftValue - rightValue
                val E = s"${expToString(exp)} evalto $result by E-Minus{"
                val inBrace = solve(left) + "\n" + solve(right) + "\n" + s"$leftValue minus $rightValue is $result by B-Minus{};"
                E + "\n" + inBrace + "\n};"
              case Asterisk =>
                val result = leftValue * rightValue
                val E = s"${expToString(exp)} evalto $result by E-Times{"
                val inBrace = solve(left) + "\n" + solve(right) + "\n" + s"$leftValue times $rightValue is $result by B-Times{};"
                E + "\n" + inBrace + "\n};\n"
              case LessThan =>
                val result = leftValue < rightValue
                val E = s"${expToString(exp)} evalto $result by E-Lt{"
                val inBrace = solve(left) + "\n" + solve(right) + "\n" + s"$leftValue less than $rightValue is $result by B-Lt{};"
                E + "\n" + inBrace + "\n};\n"
              case GreaterThan =>
                val result = leftValue > rightValue
                val E = s"${expToString(exp)} evalto $result by E-Gt{"
                val inBrace = solve(left) + "\n" + solve(right) + "\n" + s"$leftValue greater than $rightValue is $result by E-Gt{};"
                E + "\n" + inBrace + "\n};\n"
            }
        }
    }
  }

  val opMap: Map[Op, String] = Map(Plus -> "+", Minus -> "-", Asterisk -> "*", LessThan -> "<", GreaterThan -> ">")
  def expToString(exp: Exp): String = {
    exp match {
      case IntVal(n) => s"$n"
      case InfixExp(IntVal(0), Minus, right) =>
        s"-${expToString(right)}"
      case InfixExp(left, op, right) => {
        s"(${expToString(left)} ${opMap(op)} ${expToString(right)})"
      }
      case IfExp(condExp, thenExp, elseExp) => {
        s"if ${expToString(condExp)} then ${expToString(thenExp)} else ${expToString(elseExp)}"
      }
    }
  }
}