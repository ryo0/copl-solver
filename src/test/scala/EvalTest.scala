import org.scalatest.FunSuite
import tokenizer.tokenizer.tokenize
import parser.parser.{parseExp, parseMul}
import eval.eval.eval

class EvalTest extends FunSuite {
  test("eval") {
    assert(eval(parseExp(tokenize("1+2-3"))._1) === 0)
    assert(eval(parseExp(tokenize("1 + 2 * 3"))._1) === 7)
    assert(eval(parseExp(tokenize("1 + (2 * (3 - 4)) - 5"))._1) === -6)
    assert(eval(parseExp(tokenize("5 * (1 - 2 + (3-4) * 5 -7 * (11 + 1) -6) + 9 * (1 + 3)"))._1) === -444)
  }
}