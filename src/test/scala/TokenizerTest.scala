import tokenizer.tokenizer.tokenize
import tokenizer.tokenizer.tokenizeInt
import tokenizer.token._
import org.scalatest.FunSuite

class TokenizerTest extends FunSuite  {
  test("tokenizeInt") {
    assert(tokenizeInt("123)") === ( ")", IntToken(123)))
    assert(tokenizeInt("1+1") === ( "+1", IntToken(1)))
  }

  test("tokenize") {
    assert(tokenize("123)") === List(IntToken(123), RParen))
    assert(tokenize("1 + 1") === List(IntToken(1), PlusToken, IntToken(1)))
    assert(tokenize("1 + (21 - 3)") === List(IntToken(1), PlusToken, LParen, IntToken(21), MinusToken, IntToken(3), RParen))
    assert(tokenize("1 * 300") === List(IntToken(1), AsteriskToken, IntToken(300)))
  }
}
