import solver.solver.solve
import java.io.PrintWriter

import parser.ast.{EList, EmptyList, IntVal, Var, WildCard}
import tokenizer.token.ElseToken

object main extends App {
  val file41 = new PrintWriter("EvalML4Result/1.txt")
  file41.write(solve("(1 + 2) :: (3 + 4) :: []"))
  file41.close()

  val file42 = new PrintWriter("EvalML4Result/2.txt")
  file42.write(
    solve(
      "let f = fun x -> match x with [] -> 0 | a :: b -> a in f (4::[]) + f [] + f (1 :: 2 :: 3 :: [])"
    )
  )
  file42.close()

  val file43 = new PrintWriter("EvalML4Result/3.txt")
  file43.write(
    solve("let rec f = fun x -> if x < 1 then [] else x :: f (x - 1) in f 3")
  )
  file43.close()

  val file44 = new PrintWriter("EvalML4Result/4.txt")
  file44.write(
    solve(
      "let rec length = fun l -> match l with [] -> 0 | x :: y -> 1 + length y in length (1 :: 2 :: 3 :: [])"
    )
  )
  file44.close()

  val file45 = new PrintWriter("EvalML4Result/5.txt")
  file45.write(
    solve(
      "let rec length = fun l -> match l with [] -> 0 | x :: y -> 1 + length y in length ((1 :: 2 :: []) :: (3 :: 4 :: 5 :: []) :: [])"
    )
  )
  file45.close()

  val file46 = new PrintWriter("EvalML4Result/6.txt")
  file46.write(
    solve(
      "let rec append = fun l1 -> fun l2 -> match l1 with [] -> l2 | x :: y -> x :: append y l2 in append (1 :: 2 :: []) (3 :: 4 :: 5 :: [])"
    )
  )
  file46.close()

  val file47 = new PrintWriter("EvalML4Result/7.txt")
  file47.write(
    solve(
      "let rec apply = fun l -> fun x -> match l with [] -> x | f :: l -> f (apply l x) in apply ((fun x -> x * x) :: (fun y -> y + 3) :: []) 4 "
    )
  )
  file47.close()

  val file48 = new PrintWriter("EvalML4Result/8.txt")
  file48.write(
    solve(
      "let rec apply = fun l -> fun x -> match l with [] -> x | f :: l -> apply l (f x) in apply ((fun x -> x * x) :: (fun y -> y + 3) :: []) 4 "
    )
  )
  file48.close()

  val file51 = new PrintWriter("EvalML5Result/1.txt")
  file51.write(
    solve(
      "let rec max = fun l -> match l with x :: [] -> x | x :: y :: z -> if x < y then max (y :: z) else max (x :: z) in max (9 :: 2 :: 3 :: [])"
    )
  )
  file51.close()
  val file52 = new PrintWriter("EvalML5Result/2.txt")
  file52.write(
    solve(
      "let rec heads = fun l -> match l with [] -> [] | [] :: l' -> heads l' | (x :: _) :: l' -> x :: heads l' in heads ((1 :: 2 :: []) :: [] :: (3 :: []) :: [])"
    )
  )
  file52.close()
}
