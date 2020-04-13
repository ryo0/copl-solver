import parser.ast._
import parser.parser._
import tokenizer.tokenizer.tokenize
import org.scalatest.FunSuite

class ParserTest extends FunSuite {
  test("parseMul") {
    assert(parseMul(tokenize("1*2*3")) === (InfixExp
      (InfixExp(IntVal(1), Asterisk, IntVal(2)), Asterisk, IntVal(3)), List()))
    assert(parseMul(tokenize("1 * (2 * 3)")) === (InfixExp(IntVal(1),Asterisk,
      InfixExp(IntVal(2), Asterisk, IntVal(3))), List()))
    assert(parseMul(tokenize("1 * (2 * (3 * 4)) * 5")) === (
      InfixExp(
        InfixExp(IntVal(1),Asterisk,
          InfixExp(IntVal(2), Asterisk,
            InfixExp(IntVal(3), Asterisk, IntVal(4)))),
          Asterisk, IntVal(5)), List()))
  }
  test("parseExp") {
    assert(parseSum(tokenize("1+2-3")) === (InfixExp
    (InfixExp(IntVal(1), Plus, IntVal(2)), Minus, IntVal(3)), List()))
    assert(parseSum(tokenize("1 + 2 * 3")) === (InfixExp(IntVal(1),Plus,
      InfixExp(IntVal(2), Asterisk, IntVal(3))), List()))
    assert(parseSum(tokenize("1 + (2 * (3 - 4)) - 5")) === (
      InfixExp(
        InfixExp(IntVal(1),Plus,
          InfixExp(IntVal(2), Asterisk,
            InfixExp(IntVal(3), Minus, IntVal(4)))),
        Minus, IntVal(5)), List()))
  }
}
