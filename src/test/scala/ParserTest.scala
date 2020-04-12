import parser.ast._
import parser.parser.parseMul
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
}
