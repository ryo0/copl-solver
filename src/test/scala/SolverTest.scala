import org.scalatest.FunSuite
import parser.ast.Exp
import parser.parser.parseExp
import solver.typeRule
import solver.typeRule._
import tokenizer.tokenizer.tokenize
class SolverTest extends FunSuite {
  test("checkMatchin") {
    assert(
      parseExp(tokenize("(x :: _) :: l'"))._1
        .checkMatching(parseExp(tokenize("(3 :: []) :: []"))._1)
        === true
    )
  }
  test("fixTypeAnswer") {
    assert(
      fixTypeAnswer(
        List(
          (
            MLFunType(TypeVar("x"), TypeVar("y")),
            MLFunType(MLIntType, MLBoolType)
          )
        )
      ) ==
        List((TypeVar("x"), MLIntType), (TypeVar("y"), MLBoolType))
    )
  }

  test("removeDuplicationOfTypeAnswer") {
    assert(
      removeDuplicationOfTypeAnswer(
        List(
          (TypeVar("x"), TypeVar("y")),
          (TypeVar("x"), MLBoolType),
          (TypeVar("x"), TypeVar("z")),
          (TypeVar("a"), MLIntType),
          (MLBoolType, MLBoolType)
        )
      ) ==
        List(
          (TypeVar("x"), MLBoolType),
          (MLBoolType, MLBoolType),
          (TypeVar("a"), MLIntType)
        )
    )
  }
}
