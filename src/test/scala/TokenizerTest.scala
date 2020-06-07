import tokenizer.tokenizer.tokenize
import tokenizer.tokenizer.tokenizeInt
import tokenizer.token._
import org.scalatest.FunSuite
import parser.ast.IntVal

class TokenizerTest extends FunSuite {
  test("tokenizeInt") {
    assert(tokenizeInt("123)") === (")", IntToken(123)))
    assert(tokenizeInt("1+1") === ("+1", IntToken(1)))
  }

  test("varOrReservedWord") {
    assert(
      tokenize("if 1 > 3 then 1 else 3") === List(
        IfToken,
        IntToken(1),
        GreaterThanToken,
        IntToken(3),
        ThenToken,
        IntToken(1),
        ElseToken,
        IntToken(3)
      )
    )
  }

  test("let") {
    assert(
      tokenize("let x = 2 in x") === List(
        LetToken,
        VarToken("x"),
        EqualToken,
        IntToken(2),
        InToken,
        VarToken("x")
      )
    )
  }

  test("tokenize") {
    assert(tokenize("123)") === List(IntToken(123), RParen))
    assert(tokenize("1 + 1") === List(IntToken(1), PlusToken, IntToken(1)))
    assert(
      tokenize("1 + (21 - 3)") === List(
        IntToken(1),
        PlusToken,
        LParen,
        IntToken(21),
        MinusToken,
        IntToken(3),
        RParen
      )
    )
    assert(
      tokenize("1 * 300") === List(IntToken(1), AsteriskToken, IntToken(300))
    )
  }

  test("with type and eval") {
    assert(
      tokenize("x : bool, y : int |- if x then y + 1 else y - 1 : int") ===
        List(
          VarToken("x"),
          TypeSeparatorToken,
          BoolSymbolToken,
          CommaToken,
          VarToken("y"),
          TypeSeparatorToken,
          IntSymbolToken,
          EnvSeparatorToken,
          IfToken,
          VarToken("x"),
          ThenToken,
          VarToken("y"),
          PlusToken,
          IntToken(1),
          ElseToken,
          VarToken("y"),
          MinusToken,
          IntToken(1),
          TypeSeparatorToken,
          IntSymbolToken
        )
    )
  }
}
