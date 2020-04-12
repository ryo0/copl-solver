import org.scalatest.FunSuite
import tokenizer.tokenizer.tokenize
import parser.parser.parseExp
import eval.eval.eval

class EvalTest extends FunSuite {
  test("eval") {
    assert(eval(parseExp(tokenize("1+2-3"))._1) === 0)
    assert(eval(parseExp(tokenize("1 + 2 * 3"))._1) === 7)
    assert(eval(parseExp(tokenize("1 + (2 * (3 - 4)) - 5"))._1) === -6)
    assert(eval(parseExp(tokenize("5 * (1 - 2 + (3-4) * 5 -7 * (11 + 1) -6) + 9 * (1 + 3)"))._1) === -444)
    assert(eval(parseExp(tokenize("3 * -2"))._1) === -6)
    assert(eval(parseExp(tokenize("-1 + 2"))._1) === 1)
    assert(eval(parseExp(tokenize("3 + -2"))._1) === 1)
    assert(eval(parseExp(tokenize("-1 * 5 + -3* 5 + (-10 * 3 * -2 + (1 - 1* 1 - -1) * 5) + 6 -(1 + 4)"))._1) === 46)
  }
}