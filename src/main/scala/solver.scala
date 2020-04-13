import parser.ast._
import eval.eval.eval
object solver {

//  def solve(exp: Exp): String = {
//    exp match {
//      case IntVal(n) =>
//        s"$n evalto $n by E-Int{};"
//      case InfixExp(left, op, right) =>
//        val leftVal = eval(left)
//        val rightVal = eval(right)
//        op match {
//          case Plus =>
//            val result = leftVal + rightVal
//            val E =  s"${expToString(exp)} evalto $result by E-Plus{"
//            val inBrace = solve(left) + "\n" + solve(right) + "\n" +s"$leftVal plus $rightVal is $result by B-Plus{};"
//            E + "\n" + inBrace + "\n};\n"
//          case Minus =>
//            val result = leftVal - rightVal
//            val E =  s"${expToString(exp)} evalto $result by E-Minus{"
//            val inBrace = solve(left) + "\n" + solve(right) + "\n" +s"$leftVal minus $rightVal is $result by B-Minus{};"
//            E + "\n" + inBrace + "\n};"
//          case Asterisk =>
//            val result = leftVal * rightVal
//            val E =  s"${expToString(exp)} evalto $result by E-Times{"
//            val inBrace = solve(left) + "\n" + solve(right) + "\n" +s"$leftVal times $rightVal is $result by B-Times{};"
//            E + "\n" + inBrace + "\n};\n"
//        }
//    }
//  }
  val opMap: Map[Op, String] = Map(Plus -> "+", Minus -> "-", Asterisk -> "*")
  def expToString(exp: Exp): String = {
    exp match {
      case IntVal(n) => s"$n"
      case InfixExp(left, op, right) => {
        s"(${expToString(left)} ${opMap(op)} ${expToString(right)})"
      }
    }
  }
}