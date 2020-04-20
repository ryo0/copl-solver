import parser.ast._
import parser.parser.{parseExp, _}
import tokenizer.tokenizer.tokenize
import org.scalatest.FunSuite

class ParserTest extends FunSuite {
  test("parseMul") {
    assert(
      parseMul(tokenize("1*2*3")) === (InfixExp(
        InfixExp(IntVal(1), Asterisk, IntVal(2)),
        Asterisk,
        IntVal(3)
      ), List())
    )
    assert(
      parseMul(tokenize("1 * (2 * 3)")) === (InfixExp(
        IntVal(1),
        Asterisk,
        InfixExp(IntVal(2), Asterisk, IntVal(3))
      ), List())
    )
    assert(
      parseMul(tokenize("1 * (2 * (3 * 4)) * 5")) === (InfixExp(
        InfixExp(
          IntVal(1),
          Asterisk,
          InfixExp(
            IntVal(2),
            Asterisk,
            InfixExp(IntVal(3), Asterisk, IntVal(4))
          )
        ),
        Asterisk,
        IntVal(5)
      ), List())
    )
  }
  test("parseExp") {
    assert(parseSum(tokenize("2")) === (IntVal(2), List()))
    assert(
      parseSum(tokenize("1+2-3")) === (InfixExp(
        InfixExp(IntVal(1), Plus, IntVal(2)),
        Minus,
        IntVal(3)
      ), List())
    )
    assert(
      parseSum(tokenize("1 + 2 * 3")) === (InfixExp(
        IntVal(1),
        Plus,
        InfixExp(IntVal(2), Asterisk, IntVal(3))
      ), List())
    )
    assert(
      parseSum(tokenize("1 + (2 * (3 - 4)) - 5")) === (InfixExp(
        InfixExp(
          IntVal(1),
          Plus,
          InfixExp(IntVal(2), Asterisk, InfixExp(IntVal(3), Minus, IntVal(4)))
        ),
        Minus,
        IntVal(5)
      ), List())
    )
  }
  test("parseIf") {
    assert(
      parseExp(tokenize("if 1 > 0 then 1 else 0")) === (IfExp(
        InfixExp(IntVal(1), GreaterThan, IntVal(0)),
        IntVal(1),
        IntVal(0)
      ), List())
    )
    assert(
      parseExp(tokenize("(if (1 * 2) + 3 > -1 then 1 else 0) + 1")) ===
        (InfixExp(
          IfExp(
            InfixExp(
              InfixExp(
                InfixExp(IntVal(1), Asterisk, IntVal(2)),
                Plus,
                IntVal(3)
              ),
              GreaterThan,
              InfixExp(IntVal(0), Minus, IntVal(1))
            ),
            IntVal(1),
            IntVal(0)
          ),
          Plus,
          IntVal(1)
        ), List())
    )
    assert(
      parseExp(tokenize("if 1 > 0 then 1 else 0 + 1")) === (IfExp(
        InfixExp(IntVal(1), GreaterThan, IntVal(0)),
        IntVal(1),
        InfixExp(IntVal(0), Plus, IntVal(1))
      ), List())
    )
  }

  test("parseLet") {
    assert(
      parseExp(tokenize("let x = 2 in x + y")) === (LetExp(
        Var("x"),
        IntVal(2),
        InfixExp(Var("x"), Plus, Var("y"))
      ), List())
    )
  }

  test("parseFun") {
    assert(
      parseExp(tokenize("fun x -> 1")) === (FunExp(List(Var("x")), IntVal(1)), List())
    )
    assert(
      parseExp(tokenize("let f = fun y -> y * a in f")) === (LetExp(
        Var("f"),
        FunExp(List(Var("y")), InfixExp(Var("y"), Asterisk, Var("a"))),
        Var("f")
      ), List())
    )
  }

  test("parseFunCall") {
    assert(
      parseExp(tokenize("x 1")) === (FunCall(Var("x"), List(IntVal(1))), List())
    )
    parseExp(tokenize("x 1 + 2")) === (InfixExp(
      FunCall(Var("x"), List(IntVal(1))),
      Plus,
      IntVal(2)
    ), List())
    parseExp(tokenize("x a b")) === (FunCall(
      Var("x"),
      List(Var("a"), (Var("b")))
    ), List())
    parseExp(tokenize("x (1 + 2)")) === (FunCall(
      Var("x"),
      List(InfixExp(IntVal(1), Plus, IntVal(2)))
    ), List())
    parseExp(tokenize("x a (b + c)")) === (FunCall(
      Var("x"),
      List(Var("a"), (InfixExp(Var("b"), Plus, Var("c"))))
    ), List())
    parseExp(tokenize("twice (fun x -> x *x )")) === (FunCall(
      Var("twice"),
      List(FunExp(List(Var("x")), InfixExp(Var("x"), Asterisk, Var("x"))))
    ), List())
    parseExp(tokenize("let k = fun x -> fun y -> x in k 7")) === (LetExp(
      Var("k"),
      FunExp(List(Var("x")), FunExp(List(Var("y")), Var("x"))),
      FunCall(Var("k"), List(IntVal(7)))
    ), List())
  }
}
