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
          (MLBoolType, MLBoolType),
          (TypeVar("c"), TypeVar("x"))
        )
      ) ==
        List(
          (TypeVar("x"), MLBoolType),
          (MLBoolType, MLBoolType),
          (TypeVar("a"), MLIntType),
          (TypeVar("c"), MLBoolType)
        )
    )
  }
  test("normalize") {
    assert(
      normalize(
        fixTypeAnswer(
          List(
            (TypeVar("x"), TypeVar("y")),
            (TypeVar("x"), MLBoolType),
            (TypeVar("x"), TypeVar("z")),
            (TypeVar("a"), MLIntType),
            (MLBoolType, MLBoolType),
            (TypeVar("c"), TypeVar("x"))
          )
        )
      ) ==
        List(
          (TypeVar("x"), MLBoolType),
          (MLBoolType, MLBoolType),
          (TypeVar("a"), MLIntType),
          (TypeVar("c"), MLBoolType)
        )
    )
  }
  test("getAnswerRec") {
    assert(
      getAnswerRec(
        "x",
        List(
          (TypeVar("x"), TypeVar("y")),
          (TypeVar("x"), MLBoolType),
          (TypeVar("x"), TypeVar("z")),
          (TypeVar("a"), MLIntType),
          (MLBoolType, MLBoolType),
          (TypeVar("c"), TypeVar("x"))
        )
      ) === Some(MLBoolType)
    )
    assert(
      getAnswerRec(
        "a",
        List(
          (TypeVar("x"), TypeVar("y")),
          (TypeVar("x"), MLBoolType),
          (TypeVar("x"), TypeVar("z")),
          (TypeVar("a"), MLIntType),
          (MLBoolType, MLBoolType),
          (TypeVar("c"), TypeVar("x"))
        )
      ) === Some(MLIntType)
    )
    assert(
      getAnswerRec(
        "c",
        List(
          (TypeVar("x"), TypeVar("y")),
          (TypeVar("x"), MLBoolType),
          (TypeVar("x"), TypeVar("z")),
          (TypeVar("a"), MLIntType),
          (MLBoolType, MLBoolType),
          (TypeVar("c"), TypeVar("x"))
        )
      ) === Some(MLBoolType)
    )
  }
}
