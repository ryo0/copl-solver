import solver.typeSolver.typeExtract
import solver.typeSolver.typeSolveWithEnvAndType
import java.io.PrintWriter
import solver.typeRule.{MLBoolType, MLIntType}

object Main {
  def main(args: Array[String]): Unit = {
    val file81 = new PrintWriter("TypingML4Result/1.txt")
    file81.write(typeSolveWithEnvAndType("|- 3 + 5 : int"))
    file81.close()

    val file82 = new PrintWriter("TypingML4Result/2.txt")
    file82.write(
      typeSolveWithEnvAndType("|- if 4 < 5 then 2 + 3 else 8 * 8 : int")
    )
    file82.close()

    val file83 = new PrintWriter("TypingML4Result/3.txt")
    file83.write(
      typeSolveWithEnvAndType(
        "x : bool, y : int |- if x then y + 1 else y - 1 : int"
      )
    )
    file83.close()

    val file84 = new PrintWriter("TypingML4Result/4.txt")
    file84.write(
      typeSolveWithEnvAndType(
        "|- let x = 3 < 2 in let y = 5 in if x then y else 2 : int"
      )
    )
    file84.close()

    val file85 = new PrintWriter("TypingML4Result/5.txt")
    file85.write(typeSolveWithEnvAndType("|- fun x -> x + 1 : int -> int"))
    file85.close()

    val file86 = new PrintWriter("TypingML4Result/6.txt")
    file86.write(
      typeSolveWithEnvAndType("|- let f = fun x -> x + 1 in f 4 : int")
    )
    file86.close()

    val file87 = new PrintWriter("TypingML4Result/7.txt")
    file87.write(
      typeSolveWithEnvAndType("|- fun f -> f 0 + f 1 : (int -> int) -> int")
    )
    file87.close()

    val file88 = new PrintWriter("TypingML4Result/8.txt")
    file88.write(
      typeSolveWithEnvAndType(
        "|- let max = fun x -> fun y -> if x < y then y else x in max 3 5 : int"
      )
    )
    file88.close()

    val file89 = new PrintWriter("TypingML4Result/9.txt")
    file89.write(typeSolveWithEnvAndType("|- 4 :: [] : int list"))
    file89.close()

    val file810 = new PrintWriter("TypingML4Result/10.txt")
    file810.write(typeSolveWithEnvAndType("|- true :: false :: [] : bool list"))
    file810.close()

    val file811 = new PrintWriter("TypingML4Result/11.txt")
    file811.write(
      typeSolveWithEnvAndType("|- fun x -> fun y -> x : int -> int -> int")
    )
    file811.close()

    val file812 = new PrintWriter("TypingML4Result/12.txt")
    file812.write(
      typeSolveWithEnvAndType("|- fun x -> fun y -> x : bool -> int -> bool")
    )
    file812.close()

    val file813 = new PrintWriter("TypingML4Result/13.txt")
    file813.write(
      typeSolveWithEnvAndType(
        "|- let k = fun x -> fun y -> x in k 3 true : int"
      )
    )
    file813.close()

    val file814 = new PrintWriter("TypingML4Result/14.txt")
    file814.write(
      typeSolveWithEnvAndType(
        "|- let k = fun x -> fun y -> x in k (1::[]) 3 : int list"
      )
    )
    file814.close()

    val file815 = new PrintWriter("TypingML4Result/15.txt")
    file815.write(
      typeSolveWithEnvAndType(
        "|- let k = fun x -> fun y -> x in k true (fun x -> x + 1) : bool"
      )
    )
    file815.close()

    val file = new PrintWriter("TypingML4Result/demo.txt")
    file.write(
      typeSolveWithEnvAndType("|- (fun x -> fun y -> x) 1 (fun x -> x) : int")
    )
    file.close()

    val file816 = new PrintWriter("TypingML4Result/16.txt")
    file816.write(
      typeSolveWithEnvAndType(
        "|- let compose = fun f -> fun g -> fun x -> f (g x) in\n   let p = fun x -> x * x in\n   let q = fun x -> x + 4 in\n   compose p q : int -> int"
      )
    )
    file816.close()

    val file817 = new PrintWriter("TypingML4Result/17.txt")
    file817.write(
      typeSolveWithEnvAndType(
        "|- let compose = fun f -> fun g -> fun x -> f (g x) in\n   let p = fun x -> if x then 3 else 4 in\n   let q = fun x -> x < 4 in\n   compose p q : int -> int"
      )
    )
    file817.close()

    val file818 = new PrintWriter("TypingML4Result/18.txt")
    file818.write(
      typeSolveWithEnvAndType(
        "|- let s = fun f -> fun g -> fun x -> f x (g x) in\n   let k1 = fun x -> fun y -> x in\n   let k2 = fun x -> fun y -> x in\n   s k1 k2 : int -> int"
      )
    )
    file818.close()

    val fileDemo2 = new PrintWriter("TypingML4Result/demo2.txt")
    fileDemo2.write(
      typeSolveWithEnvAndType(
        "|- let s = fun f -> fun g -> fun x -> f x (g x) in s : ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c "
      )
    )
    fileDemo2.close()

    val file819 = new PrintWriter("TypingML4Result/19.txt")
    file819.write(
      typeSolveWithEnvAndType(
        "|- let s = fun f -> fun g -> fun x -> f x (g x) in\n   let k1 = fun x -> fun y -> x in\n   let k2 = fun x -> fun y -> x in\n   s k1 k2 (fun x -> x + 1) : int -> int"
      )
    )
    file819.close()

//
//    val s = "let s = fun f -> fun g -> fun x -> f x (g x) in s "
//    val t = tokenizer.tokenizer.tokenize(s)
//    val p = parser.parser.parseExp(t)._1
//    println(p.getTypeWithoutAnswer(List()).string())

    val file820 = new PrintWriter("TypingML4Result/20.txt")
    file820.write(
      typeSolveWithEnvAndType(
        "|- let rec fact = fun n ->\n     if n < 2 then 1 else n * fact (n - 1) in\n     fact 3 : int"
      )
    )
    file820.close()

    //  val file821 = new PrintWriter("TypingML4Result/4.txt")
    //  file821.write(typeSolveWithEnvAndType("let x = 3 < 2 in let y = 5 in if x then y else 2"))
    //  file821.close()
    //
    //  val file822 = new PrintWriter("TypingML4Result/4.txt")
    //  file822.write(typeSolveWithEnvAndType("let x = 3 < 2 in let y = 5 in if x then y else 2"))
    //  file822.close()
    //
    //  val file823 = new PrintWriter("TypingML4Result/4.txt")
    //  file823.write(typeSolveWithEnvAndType("let x = 3 < 2 in let y = 5 in if x then y else 2"))
    //  file823.close()
    //
    //  val file824 = new PrintWriter("TypingML4Result/4.txt")
    //  file824.write(typeSolveWithEnvAndType("let x = 3 < 2 in let y = 5 in if x then y else 2"))
    //  file824.close()
    //
    //  val file825 = new PrintWriter("TypingML4Result/4.txt")
    //  file825.write(typeSolveWithEnvAndType("let x = 3 < 2 in let y = 5 in if x then y else 2"))
    //  file825.close()
    //
    //  val file826 = new PrintWriter("TypingML4Result/4.txt")
    //  file826.write(typeSolveWithEnvAndType("let x = 3 < 2 in let y = 5 in if x then y else 2"))
    //  file826.close()
    //
    //  val file827 = new PrintWriter("TypingML4Result/4.txt")
    //  file827.write(typeSolveWithEnvAndType("let x = 3 < 2 in let y = 5 in if x then y else 2"))
    //  file827.close()
  }
}
