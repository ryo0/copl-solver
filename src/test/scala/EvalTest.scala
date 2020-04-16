import org.scalatest.FunSuite
import tokenizer.tokenizer.tokenize
import parser.parser.{parseExp, parseSum}
import eval.eval.initEval
import parser.ast.{Asterisk, GreaterThan, IfExp, InfixExp, IntVal, Minus, Plus}

class EvalTest extends FunSuite {
  test("eval") {
    assert(initEval(parseSum(tokenize("1+2-3"))._1) === IntVal(0))
    assert(initEval(parseSum(tokenize("1 + 2 * 3"))._1) === IntVal(7))
    assert(initEval(parseSum(tokenize("1 + (2 * (3 - 4)) - 5"))._1) === IntVal(-6))
    assert(initEval(parseSum(tokenize("5 * (1 - 2 + (3-4) * 5 -7 * (11 + 1) -6) + 9 * (1 + 3)"))._1) === IntVal(-444))
    assert(initEval(parseSum(tokenize("3 * -2"))._1) === IntVal(-6))
    assert(initEval(parseSum(tokenize("-1 + 2"))._1) === IntVal(1))
    assert(initEval(parseSum(tokenize("3 + -2"))._1) === IntVal(1))
    assert(initEval(parseSum(tokenize("-1 * 5 + -3* 5 + (-10 * 3 * -2 + (1 - 1* 1 - -1) * 5) + 6 -(1 + 4)"))._1) ===IntVal(46))
    assert(initEval(parseSum(tokenize("-10 / 5)"))._1) === IntVal(-2))
    assert(initEval(parseSum(tokenize("10 / -(2*3 -1))"))._1) === IntVal(-2))
    assert(initEval(parseSum(tokenize("8 - 2 - 3"))._1) === IntVal(3))
  }

  test("evalIf") {
    assert(initEval(parseExp(tokenize("if 1 > 0 then 1 else 0"))._1) === IntVal(1))
    assert(initEval(parseExp(tokenize("(if (1 * 2) + 3 > -1 then 1 else 0) + 1"))._1) === IntVal(2))
    assert(initEval(parseExp(tokenize("if 1 > 0 then 1 else 0 + 1"))._1) === IntVal(1))
  }


  test("let") {
    assert(initEval(parseExp(tokenize("let x = 1 in x + 1"))._1) === IntVal(2))
    assert(initEval(parseExp(tokenize("let x = 1 in let y = 2 in x + y * 2"))._1) === IntVal(5))
    assert(initEval(parseExp(tokenize("(let x = 1 in let y = 2 in x + y * 2) * 2"))._1) === IntVal(10))
  }
}