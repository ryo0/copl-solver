package solver
import parser.parser._
import tokenizer.tokenizer.tokenize
import typeRule._
object typeSolver {
  def typeSolveWithEnvAndType(string: String): String = {
    val tokens = tokenize(string)
    val (typeEnv, rest1) = parseTypeEnv(tokens)
    val (exp, rest2) = parseExp(rest1)
    val (t, _) = parseType(rest2)
    val solved = exp.typeSolve(typeEnv, t)
    solved.string(0)
    val typeAnswer = getTypeAnswer(solved)
    println(solved.string(0))
    println(typeAnswer)
    solved.substitute(typeAnswer).fillTypeVar().string(0)
  }
  def typeExtract(string: String): String = {
    val exp = parser.parser.parseExp(tokenize(string))._1
    val result = exp.typeExtract(List())
    result._1.toString() + result._2.toString
  }

}
