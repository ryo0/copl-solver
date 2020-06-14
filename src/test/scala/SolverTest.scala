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
          ),
          (
            MLFunType(MLFunType(TypeVar("a"), TypeVar("b")), TypeVar("c")),
            MLFunType(MLFunType(MLIntType, MLBoolType), MLIntType)
          ),
          (TypeVar("z"), MLFunType(TypeVar("k"), TypeVar("l"))),
          (TypeVar("n"), TypeVar("x")),
          (TypeVar("j"), MLIntType)
        )
      ).sortBy(a => a._1.string()) ==
        List(
          (TypeVar("x"), MLIntType),
          (TypeVar("y"), MLBoolType),
          (TypeVar("a"), MLIntType),
          (TypeVar("b"), MLBoolType),
          (TypeVar("c"), MLIntType),
          (TypeVar("z"), MLFunType(TypeVar("k"), TypeVar("l"))),
          (TypeVar("n"), TypeVar("x")),
          (TypeVar("j"), MLIntType)
        ).sortBy(a => a._1.string())
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
      ).sortBy(a => a._1.string()) ==
        List(
          (TypeVar("x"), MLBoolType),
          (MLBoolType, MLBoolType),
          (TypeVar("a"), MLIntType),
          (TypeVar("c"), MLBoolType)
        ).sortBy(a => a._1.string())
    )
    assert(
      normalize(
        List(
          (TypeVar("x"), TypeVar("y")),
          (TypeVar("a"), MLIntType),
          (TypeVar("'x144"), TypeVar("'x")),
          (MLBoolType, MLBoolType),
          (TypeVar("c"), TypeVar("x")),
          (TypeVar("'x"), TypeVar("'x144")),
          (TypeVar("'x"), TypeVar("'x")),
          (TypeVar("x"), MLBoolType),
          (TypeVar("x"), TypeVar("z")),
          (TypeVar("'x144"), TypeVar("'x144")),
          (TypeVar("'x"), MLFunType(MLIntType, MLIntType)),
          (TypeVar("x"), TypeVar("z")),
          (MLFunType(MLIntType, MLIntType), TypeVar("'x")),
          (TypeVar("'x144"), TypeVar("'x166")),
          (TypeVar("'x166"), TypeVar("'x144")),
        )
      ).sortBy(a => a._1.string()) === List(
        (TypeVar("'x144"), MLFunType(MLIntType, MLIntType)),
        (TypeVar("'x166"), MLFunType(MLIntType, MLIntType)),
        (TypeVar("'x"), MLFunType(MLIntType, MLIntType)),
        (TypeVar("x"), MLBoolType),
        (MLBoolType, MLBoolType),
        (TypeVar("a"), MLIntType),
        (TypeVar("c"), MLBoolType),
        (MLFunType(MLIntType, MLIntType), TypeVar("'x"))
      ).sortBy(a => a._1.string())
    )
    assert {
      normalize(
        List(
          (TypeVar("x158"), TypeVar("x180")),
          (TypeVar("a"), MLIntType),
          (TypeVar("x180"), TypeVar("x158")),
          (MLBoolType, MLBoolType),
          (TypeVar("x172"), TypeVar("x158")),
          (TypeVar("x158"), TypeVar("x172")),
          (TypeVar("x158"), TypeVar("x158")),
          (TypeVar("x"), MLBoolType),
          (TypeVar("x158"), TypeVar("x158")),
          (TypeVar("x144"), TypeVar("x144")),
          (TypeVar("x172"), MLFunType(MLIntType, MLIntType)),
          (TypeVar("x"), TypeVar("z")),
          (MLFunType(MLIntType, MLIntType), TypeVar("'x")),
          (TypeVar("x144"), TypeVar("x166")),
          (TypeVar("x166"), TypeVar("x144")),
        )
      ) == List()
    }
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
        "'x144",
        List(
          (TypeVar("'x144"), TypeVar("'x")),
          (TypeVar("'x"), TypeVar("'x144")),
          (TypeVar("'x144"), TypeVar("'x144")),
          (TypeVar("'x"), MLFunType(MLIntType, MLIntType)),
        )
      ) === Some(MLFunType(MLIntType, MLIntType))
    )
    assert(
      getAnswerRec(
        "'x144",
        List(
          (TypeVar("x"), TypeVar("y")),
          (TypeVar("a"), MLIntType),
          (TypeVar("'x144"), TypeVar("'x")),
          (MLBoolType, MLBoolType),
          (TypeVar("c"), TypeVar("x")),
          (TypeVar("'x"), TypeVar("'x144")),
          (TypeVar("'x"), TypeVar("'x")),
          (TypeVar("'x144"), TypeVar("'x144")),
          (TypeVar("x"), MLBoolType),
          (TypeVar("x"), TypeVar("z")),
          (TypeVar("'x"), MLFunType(MLIntType, MLIntType)),
          (TypeVar("x"), TypeVar("z")),
          (MLFunType(MLIntType, MLIntType), TypeVar("'x")),
        )
      ) === Some(MLFunType(MLIntType, MLIntType))
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
    assert {
      getAnswerRec(
        "x158",
        List(
          (TypeVar("x158"), TypeVar("x180")),
          (TypeVar("a"), MLIntType),
          (TypeVar("x180"), TypeVar("x158")),
          (MLBoolType, MLBoolType),
          (TypeVar("x172"), TypeVar("x158")),
          (TypeVar("x158"), TypeVar("x172")),
          (TypeVar("x158"), TypeVar("x158")),
          (TypeVar("x"), MLBoolType),
          (TypeVar("x158"), TypeVar("x158")),
          (TypeVar("x144"), TypeVar("x144")),
          (TypeVar("x172"), MLFunType(MLIntType, MLIntType)),
          (TypeVar("x"), TypeVar("z")),
          (MLFunType(MLIntType, MLIntType), TypeVar("'x")),
          (TypeVar("x144"), TypeVar("x166")),
          (TypeVar("x166"), TypeVar("x144")),
        )
      ) === Some(MLFunType(MLIntType, MLIntType))
    }
  }
}
