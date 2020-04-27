import parser.ast._
import solver.solver.{initSolve, solve, solveFunExp}
import tokenizer.tokenizer.tokenize
import parser.parser.parseExp
import java.io.PrintWriter

object main extends App {

//  println(parseExp(tokenize("3 + 5"))._1)
//  println(initSolve(parseExp(tokenize("3 + 5"))._1))
//  println(initSolve(parseExp(tokenize("8 - 2 - 3"))._1))
//  println(initSolve(parseExp(tokenize("(4 + 5) * (1 - 10) "))._1))
//  println(initSolve(parseExp(tokenize("(4 + 5) > (1 - 10) "))._1))
//
//  println(initSolve(parseExp(tokenize("if 4 < 5 then 2 + 3 else 8 * 8"))._1))
//
//  println(
//    initSolve(parseExp(tokenize("3 + if -23 < -2 * 8 then 8 else 2 + 4 "))._1)
//  )
//
//  println(
//    initSolve(parseExp(tokenize("3 + (if -23 < -2 * 8 then 8 else 2) + 4 "))._1)
//  )
//
//  println(
//    solve(parseExp(tokenize("x"))._1, List(("y", IntVal(2)), ("x", IntVal(3))))
//  )
//
//  println(
//    solve(
//      parseExp(tokenize("if x then y + 1 else y - 1"))._1,
//      List(("y", IntVal(4)), ("x", BoolVal(true)))
//    )
//  )
//
//  println(solve(parseExp(tokenize("let x = 1 + 2 in x * 4"))._1, List()))
//
//  println(
//    solve(
//      parseExp(tokenize("let x = 3 * 3 in let y = 4 * x in x + y"))._1,
//      List()
//    )
//  )
//
//  println(
//    solve(
//      parseExp(tokenize("let x = x * 2 in x + x"))._1,
//      List(("x", IntVal(3)))
//    )
//  )
//
//  println(
//    solve(
//      parseExp(tokenize("let x = let y = 3 - 2 in y * y in let y = 4 in x + y"))._1,
//      List()
//    )
//  )
  val file0 = new PrintWriter("0-correct.txt")
  file0.write(solve(parseExp(tokenize("fun x -> x + 1"))._1, List()))
  file0.close()

  val file1 = new PrintWriter("1-correct.txt")
  file1.write(
    solve(
      parseExp(tokenize("let sq = fun x -> x * x in sq 3 + sq 4 "))._1,
      List()
    )
  )
  file1.close()

  val file2 = new PrintWriter("2-correct.txt")

  file2.write(
    solve(
      parseExp(tokenize("let sq = fun x -> x * x in sq 3 + sq 4 "))._1,
      List()
    )
  )

  file2.close()

  val file3 = new PrintWriter("3-wrong.txt")

  file3.write(
    solve(
      parseExp(tokenize(" let sm = fun f -> f 3 + f 4 in sm (fun x -> x * x)"))._1,
      List()
    )
  )
  file3.close()

  val file4 = new PrintWriter("4-correct.txt")

  file4.write(
    solve(
      parseExp(
        tokenize("let a = 3 in let f = fun y -> y * a in let a = 5 in f 4")
      )._1,
      List()
    )
  )
  file4.close()

  // 正しい
  println(
    solveFunExp(
      FunExp(Var("x"), InfixExp(Var("x"), Plus, Var("y"))),
      List(IntVal(1)),
      List(("y", IntVal(5)))
    )
  )
  // 正しい
  println(
    solveFunExp(
      FunExp(Var("f"), FunCall(Var("f"), List(Var("y")))),
      List(FunExp(Var("y"), InfixExp(IntVal(2), Asterisk, Var("y")))),
      List(("y", IntVal(5)))
    )
  )
  println("---")
  //正しい
  val fileSFE1 = new PrintWriter("SFE1-correct.txt")
  fileSFE1.write(
    solveFunExp(
      FunExp(Var("f"), FunCall(Var("f"), List(Var("x"), Var("y")))),
      List(
        FunExp(
          Var("p"),
          FunExp(Var("q"), InfixExp(Var("p"), Asterisk, Var("q")))
        )
      ),
      List(("x", IntVal(3)), ("y", IntVal(7)))
    )
  )
  fileSFE1.close()
  println("----")
// だめ
  val fileSFE2 = new PrintWriter("SFE2-wrong.txt")
  fileSFE2.write(
    solve(
      parseExp(tokenize("f x y"))._1,
      List(
        (
          "f",
          FunExp(
            Var("p"),
            FunExp(Var("q"), InfixExp(Var("p"), Asterisk, Var("q")))
          )
        ),
        ("x", IntVal(3)),
        ("y", IntVal(7))
      )
    )
  )
  fileSFE2.close()

//  println("---")
//  println(
//    solve(
//      parseExp(tokenize("x * y"))._1,
//      List(("x", IntVal(3)), ("y", IntVal(7)))
//    )
//  )
  val file5 = new PrintWriter("5-correct.txt")

  file5.write(
    solve(parseExp(tokenize("let a = 1 in let b = 2 in fun x -> 1"))._1, List())
  )
  file5.close()

  val file6 = new PrintWriter("6-correct.txt")

  file6.write(
    solve(
      parseExp(tokenize("let f = fun x -> fun y -> x + y in f 1 2"))._1,
      List()
    )
  )
  file6.close()

}
