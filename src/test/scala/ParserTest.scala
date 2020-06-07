import parser.ast.{Plus, _}
import parser.parser.{parseExp, _}
import tokenizer.tokenizer.tokenize
import org.scalatest.FunSuite
import solver.typeRule.{MLBoolType, MLFunType, MLIntType, MLListType}
import tokenizer.token.{ConsToken, ElseToken, LParen, RParen, VarToken}

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

  test("parseLetRec") {
    assert(
      parseExp(tokenize("let rec f = fun x -> x + 1 in f 1")) === (LetRecExp(
        Var("f"),
        RecFunExp(Var("f"), Var("x"), InfixExp(Var("x"), Plus, IntVal(1))),
        FunCall(Var("f"), IntVal(1))
      ), List())
    )
  }

  test("parseFun") {
    assert(
      parseExp(tokenize("fun x -> 1")) === (FunExp(Var("x"), IntVal(1)), List())
    )
    assert(
      parseExp(tokenize("let f = fun y -> y * a in f")) === (LetExp(
        Var("f"),
        FunExp(Var("y"), InfixExp(Var("y"), Asterisk, Var("a"))),
        Var("f")
      ), List())
    )
  }

  test("parseFunCall") {
    assert(parseExp(tokenize("x 1")) === (FunCall(Var("x"), IntVal(1)), List()))
    assert(
      parseExp(tokenize("x 1 + 2")) === (InfixExp(
        FunCall(Var("x"), IntVal(1)),
        Plus,
        IntVal(2)
      ), List())
    )
    assert(
      parseExp(tokenize("x a b")) === (FunCall(
        FunCall(Var("x"), Var("a")),
        Var("b")
      ), List())
    )
    assert(
      parseExp(tokenize("x (1 + 2)")) === (FunCall(
        Var("x"),
        InfixExp(IntVal(1), Plus, IntVal(2))
      ), List())
    )
    assert(
      parseExp(tokenize("x a (b + c)")) === (FunCall(
        FunCall(Var("x"), Var("a")),
        InfixExp(Var("b"), Plus, Var("c"))
      ), List())
    )
    assert(
      parseExp(tokenize("twice (fun x -> x * x )")) === (FunCall(
        Var("twice"),
        FunExp(Var("x"), InfixExp(Var("x"), Asterisk, Var("x")))
      ), List())
    )
    assert(
      parseExp(tokenize("let k = fun x -> fun y -> x in k 7")) === (LetExp(
        Var("k"),
        FunExp(Var("x"), FunExp(Var("y"), Var("x"))),
        FunCall(Var("k"), IntVal(7))
      ), List())
    )
    assert(
      parseExp(tokenize("(fun x -> x * 2) 2")) === (FunCall(
        FunExp(Var("x"), InfixExp(Var("x"), Asterisk, IntVal(2))),
        IntVal(2)
      ), List())
    )
    assert(
      parseExp(tokenize("twice (fun x -> x * x) 2")) === (FunCall(
        FunCall(
          Var("twice"),
          FunExp(Var("x"), InfixExp(Var("x"), Asterisk, Var("x")))
        ),
        IntVal(2)
      ), List())
    )

    assert(
      parseExp(tokenize("f (f x)")) === (FunCall(
        Var("f"),
        FunCall(Var("f"), Var("x"))
      ),
      List())
    )

    assert(
      parseExp(tokenize("f ((fun x -> x) x)")) === (FunCall(
        Var("f"),
        FunCall(FunExp(Var("x"), Var("x")), Var("x"))
      ),
      List())
    )

    assert(
      parseExp(tokenize("f (f (f x))")) === (FunCall(
        Var("f"),
        FunCall(Var("f"), FunCall(Var("f"), Var("x")))
      ),
      List())
    )

    assert(
      parseExp(tokenize("f 1 2")) === (FunCall(
        FunCall(Var("f"), IntVal(1)),
        IntVal(2)
      ), List())
    )
    assert(
      parseExp(tokenize("fib (n - 1) + fib (n - 2)")) === (InfixExp(
        FunCall(Var("fib"), InfixExp(Var("n"), Minus, IntVal(1))),
        Plus,
        FunCall(Var("fib"), InfixExp(Var("n"), Minus, IntVal(2)))
      ), List())
    )

    assert(
      parseExp(tokenize("f [] + f (1 :: 2 :: 3 :: [])")) === (InfixExp(
        FunCall(Var("f"), EmptyList),
        Plus,
        FunCall(
          Var("f"),
          EList(IntVal(1), EList(IntVal(2), EList(IntVal(3), EmptyList)))
        )
      ), List())
    )
    assert(
      parseExp(tokenize("f (4::[]) + f [] + f (1 :: 2 :: 3 :: [])")) === (InfixExp(
        InfixExp(
          FunCall(Var("f"), EList(IntVal(4), EmptyList)),
          Plus,
          FunCall(Var("f"), EmptyList),
        ),
        Plus,
        FunCall(
          Var("f"),
          EList(IntVal(1), EList(IntVal(2), EList(IntVal(3), EmptyList)))
        )
      ), List())
    )
    assert(
      parseExp(tokenize("let k = fun x -> fun y -> x in k 3 true"))._1 === LetExp(
        Var("k"),
        FunExp(Var("x"), FunExp(Var("y"), Var("x"))),
        FunCall(FunCall(Var("k"), IntVal(3)), BoolVal(true))
      )
    )

    assert(
      parseExp(tokenize("append (1 :: []) (3 :: [])")) === (FunCall(
        FunCall(Var("append"), EList(IntVal(1), EmptyList)),
        EList(IntVal(3), EmptyList)
      ), List())
    )

    assert(
      parseExp(tokenize(" apply ((fun x -> x * x) :: (fun y -> y + 3))")) === (
        FunCall(
          Var("apply"),
          EList(
            FunExp(Var("x"), InfixExp(Var("x"), Asterisk, Var("x"))),
            FunExp(Var("y"), InfixExp(Var("y"), Plus, IntVal(3)))
          )
        ),
        List()
      )
    )

    assert(
      parseExp(tokenize("apply ((fun x -> x * x) :: (fun y -> y + 3) :: []) 4")) === (
        FunCall(
          FunCall(
            Var("apply"),
            EList(
              FunExp(Var("x"), InfixExp(Var("x"), Asterisk, Var("x"))),
              EList(
                FunExp(Var("y"), InfixExp(Var("y"), Plus, IntVal(3))),
                EmptyList
              )
            )
          ),
          IntVal(4)
        ),
        List()
      )
    )
  }
  test("getTokensInParen") {
    assert(
      getTokensInParen(tokenize("(a b c) :: (d e f)")) === (List(
        LParen,
        VarToken("a"),
        VarToken("b"),
        VarToken("c"),
        RParen
      ),
      List(
        ConsToken,
        LParen,
        VarToken("d"),
        VarToken("e"),
        VarToken("f"),
        RParen
      ))
    )
  }

  test("EList") {
    assert(
      parseExp(tokenize(" 1 :: 2 :: []")) === (EList(
        IntVal(1),
        EList(IntVal(2), EmptyList)
      ), List())
    )
    assert(
      parseExp(tokenize(" 1 :: []")) === (EList(IntVal(1), EmptyList), List())
    )
    assert(
      parseExp(tokenize("(1 + 2) :: (3 + 4) :: (5 * 6) :: []")) === (EList(
        InfixExp(IntVal(1), Plus, IntVal(2)),
        EList(
          InfixExp(IntVal(3), Plus, IntVal(4)),
          EList(InfixExp(IntVal(5), Asterisk, IntVal(6)), EmptyList)
        )
      ), List())
    )
    assert(parseExp(tokenize("x :: y")) === (EList(Var("x"), Var("y")), List()))
  }

  test("match") {
    assert(
      parseExp(tokenize("match x with [] -> 0 | a :: b -> a"))._1 === Match(
        Var("x"),
        List(
          Pattern(EmptyList, IntVal(0)),
          Pattern(EList(Var("a"), Var("b")), Var("a"))
        )
      )
    )
    assert(
      parseExp(tokenize("match l1 with [] -> l2 | x :: y -> x :: append y l2"))._1 === Match(
        Var("l1"),
        List(
          Pattern(EmptyList, Var("l2")),
          Pattern(
            EList(Var("x"), Var("y")),
            EList(
              Var("x"),
              FunCall(FunCall(Var("append"), Var("y")), Var("l2"))
            )
          )
        )
      )
    )
  }
  test("parseType") {
    assert(parseType(tokenize("int"))._1 === MLIntType)
    assert(parseType(tokenize("bool"))._1 === MLBoolType)
    assert(
      parseType(tokenize("int -> int"))._1 === MLFunType(MLIntType, MLIntType)
    )
    assert(
      parseType(tokenize("int -> int -> int"))._1 === MLFunType(
        MLIntType,
        MLFunType(MLIntType, MLIntType)
      )
    )
    assert(
      parseType(tokenize("(int -> int) -> int"))._1 === MLFunType(
        MLFunType(MLIntType, MLIntType),
        MLIntType
      )
    )
    assert(parseType(tokenize("int list"))._1 === MLListType(MLIntType))

    assert(
      parseType(tokenize("(int list -> int list) -> bool list"))._1 ===
        MLFunType(
          MLFunType(MLListType(MLIntType), MLListType(MLIntType)),
          MLListType(MLBoolType)
        )
    )
  }
}
