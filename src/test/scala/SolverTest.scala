import org.scalatest.FunSuite
import parser.ast.Exp
import parser.parser.parseExp
import tokenizer.tokenizer.tokenize
class SolverTest extends FunSuite {
  test("checkMatchin") {
    assert(
      parseExp(tokenize("(x :: _) :: l'"))._1
        .checkMatching(parseExp(tokenize("(3 :: []) :: []"))._1)
        === true
    )
  }
}
