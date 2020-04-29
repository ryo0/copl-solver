import solver.solver.solve
import java.io.PrintWriter

object main extends App {
//  file1.write(solve("3 + 5"))
//  file1.write(solve("8 - 2 - 3"))
//  file1.write(solve("(4 + 5)) * (1 - 10)")
//  file1.write(solve("1+1 < (10-7))")
//  file1.write(solve("if 4 < 5 then 2 + 3 else 8 * 8"))
//  file1.write(solve("3 + if -23 < -2 * 8 then 8 else 2 + 4"))
//  file1.write(solve("3 + (if -23 < -2 * 8 then 8 else 2)) + 4")
//  file1.write(solve("let x = 3 in let y = 2 in x"))
//  file1.write(solve("let x = true in let y = 4 in if x then y + 1 else y - 1"))
//  file1.write(solve("let x = 1 + 2 in x * 4"))
//  file1.write(solve("let x = 3 * 3 in let y = 4 * x in x + y"))
//  file1.write(solve("let x = 3 in let x = x * 2 in x + x"))
//  file1.write(solve("let x = let y = 3 - 2 in y * y in let y = 4 in x + y"))
  val file1 = new PrintWriter("1.txt")
  file1.write(solve("fun x -> x + 1"))
  file1.close()

  val file2 = new PrintWriter("2.txt")
  file2.write(solve("let y = 2 in fun x -> x + y"))
  file2.close()

  val file3 = new PrintWriter("3.txt")
  file3.write(solve("let sq = fun x -> x * x in sq 3 + sq 4"))
  file3.close()

  val file4 = new PrintWriter("4.txt")
  file4.write(solve("let sm = fun f -> f 3 + f 4 in sm (fun x -> x * x))"))
  file4.close()

  val file5 = new PrintWriter("5.txt")
  file5.write(
    solve("let max = fun x -> fun y -> if x < y then y else x in max 3 5")
  )
  file5.close()

  val file6 = new PrintWriter("6.txt")
  file6.write(solve("let a = 3 in let f = fun y -> y * a in let a = 5 in f 4"))
  file6.close()

  val file7 = new PrintWriter("7.txt")
  file7.write(
    solve("let twice = fun f -> fun x -> f (f x) in twice (fun x -> x * x) 2")
  )
  file7.close()

  val file8 = new PrintWriter("8.txt")
  file8.write(
    solve(
      "let twice = fun f -> fun x -> f (f x) in twice twice (fun x -> x * x) 2"
    )
  )
  file8.close()

  val file9 = new PrintWriter("9.txt")
  file9.write(
    solve(
      "let compose = fun f -> fun g -> fun x -> f (g x) in let p = fun x -> x * x in let q = fun x -> x + 4 in compose p q 4 "
    )
  )
  file9.close()

  val file10 = new PrintWriter("10.txt")
  file10.write(
    solve(
      "let s = fun f -> fun g -> fun x -> f x (g x) in  let k = fun x -> fun y -> x in s k k 7"
    )
  )
  file10.close()

  val file11 = new PrintWriter("11.txt")
  file11.write(
    solve(
      "let fact = fun self -> fun n -> if n < 2 then 1 else n * self self (n - 1) in fact fact 3"
    )
  )
  file11.close()
}
