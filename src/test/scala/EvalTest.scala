import org.scalatest.FunSuite
import tokenizer.tokenizer.tokenize
import parser.parser.{parseExp, parseSum}
import eval.eval.eval
import parser.ast.{Asterisk, GreaterThan, IfExp, InfixExp, IntVal, Minus, Plus}

class EvalTest extends FunSuite {
  test("eval") {
    assert(eval(parseSum(tokenize("1+2-3"))._1) === IntVal(0))
    assert(eval(parseSum(tokenize("1 + 2 * 3"))._1) === IntVal(7))
    assert(eval(parseSum(tokenize("1 + (2 * (3 - 4)) - 5"))._1) === IntVal(-6))
    assert(eval(parseSum(tokenize("5 * (1 - 2 + (3-4) * 5 -7 * (11 + 1) -6) + 9 * (1 + 3)"))._1) === IntVal(-444))
    assert(eval(parseSum(tokenize("3 * -2"))._1) === IntVal(-6))
    assert(eval(parseSum(tokenize("-1 + 2"))._1) === IntVal(1))
    assert(eval(parseSum(tokenize("3 + -2"))._1) === IntVal(1))
    assert(eval(parseSum(tokenize("-1 * 5 + -3* 5 + (-10 * 3 * -2 + (1 - 1* 1 - -1) * 5) + 6 -(1 + 4)"))._1) ===IntVal(46))
    assert(eval(parseSum(tokenize("-10 / 5)"))._1) === IntVal(-2))
    assert(eval(parseSum(tokenize("10 / -(2*3 -1))"))._1) === IntVal(-2))
    assert(eval(parseSum(tokenize("8 - 2 - 3"))._1) === IntVal(3))
  }

  test("evalIf") {
    assert(eval(parseExp(tokenize("if 1 > 0 then 1 else 0"))._1) === IntVal(1))
    assert(eval(parseExp(tokenize("(if (1 * 2) + 3 > -1 then 1 else 0) + 1"))._1) === IntVal(2))
    assert(parseExp(tokenize("if 1 > 0 then 1 else 0 + 1")) === (IfExp(InfixExp(IntVal(1), GreaterThan, IntVal(0)), IntVal(1), InfixExp(IntVal(0), Plus, IntVal(1))), List()))
  }
}