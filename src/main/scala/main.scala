import solver.solver.solve
import eval.eval.eval
import tokenizer.tokenizer.tokenize
import parser.parser.parseExp
object main extends App {
//  println(solve("3 + 5"))
//  println(solve("8 - 2 - 3"))
//  println(solve("(4 + 5) * (1 - 10)"))
//  println(solve("1+1 < (10-7)"))
//  println(solve("if 4 < 5 then 2 + 3 else 8 * 8"))
//  println(solve("3 + if -23 < -2 * 8 then 8 else 2 + 4"))
//  println(solve("3 + (if -23 < -2 * 8 then 8 else 2) + 4"))
//  println(solve("let x = 3 in let y = 2 in x"))
//  println(solve("let x = true in let y = 4 in if x then y + 1 else y - 1"))
//  println(solve("let x = 1 + 2 in x * 4"))
//  println(solve("let x = 3 * 3 in let y = 4 * x in x + y"))
//  println(solve("let x = 3 in let x = x * 2 in x + x"))
//  println(solve("let x = let y = 3 - 2 in y * y in let y = 4 in x + y"))
//  println(solve("fun x -> x + 1"))
//  println("//-------------------------------------------")
//  println(solve("let y = 2 in fun x -> x + y"))
//  println("//-------------------------------------------")
//  println(solve("let sq = fun x -> x * x in sq 3 + sq 4"))
//  println("//-------------------------------------------")
//  println(solve("let sm = fun f -> f 3 + f 4 in sm (fun x -> x * x)"))
//  println("//-------------------------------------------")
//  println(
//    solve("let max = fun x -> fun y -> if x < y then y else x in max 3 5")
//  )
//  println("//-------------------------------------------")
//  println(solve("let a = 3 in let f = fun y -> y * a in let a = 5 in f 4"))
  println("//46-------------------------------------------")
  println(
    eval(
      parseExp(
        tokenize(
          "let twice = fun f -> fun x -> f (f x) in twice (fun x -> x * x) 2"
        )
      )._1,
      List()
    )
  )
  println(
    solve("let twice = fun f -> fun x -> f (f x) in twice (fun x -> x * x) 2")
  )
//  println("//-------------------------------------------")
//  println(
//    solve(
//      "let twice = fun f -> fun x -> f (f x) in twice twice (fun x -> x * x) 2"
//    )
//  )
//  println("//-------------------------------------------")
//  println(
//    solve(
//      "let compose = fun f -> fun g -> fun x -> f (g x) in let p = fun x -> x * x in let q = fun x -> x + 4 in compose p q 4 "
//    )
//  )
//  println("//-------------------------------------------")
//  println(
//    solve(
//      "let s = fun f -> fun g -> fun x -> f x (g x) in  let k = fun x -> fun y -> x in s k k 7"
//    )
//  )
}
