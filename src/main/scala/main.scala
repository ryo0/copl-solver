import solver.typeSolver.typeSolve
import solver.typeSolver.typeExtract
import solver.typeSolver.typeSolveInEnv
import java.io.PrintWriter
import solver.typeRule.{MLBoolType, MLIntType}

object main extends App {
  val file81 = new PrintWriter("TypingML4Result/1.txt")
  file81.write(typeSolve("3 + 5"))
  file81.close()

  val file82 = new PrintWriter("TypingML4Result/2.txt")
  file82.write(typeSolve("if 4 < 5 then 2 + 3 else 8 * 8"))
  file82.close()

  val file83 = new PrintWriter("TypingML4Result/3.txt")
  file83.write(
    typeSolveInEnv(
      List(("y", MLIntType), ("x", MLBoolType)),
      "if x then y + 1 else y - 1"
    )
  )
  file83.close()

  val file84 = new PrintWriter("TypingML4Result/4.txt")
  file84.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
  file84.close()

  val file85 = new PrintWriter("TypingML4Result/5.txt")
  file85.write(typeSolve("fun x -> x + 1"))
  file85.close()

  val file86 = new PrintWriter("TypingML4Result/6.txt")
  file86.write(typeSolve("let f = fun x -> x + 1 in f 4"))
  file86.close()

//  val file87 = new PrintWriter("TypingML4Result/4.txt")
//  file87.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file87.close()
//
//  val file88 = new PrintWriter("TypingML4Result/4.txt")
//  file88.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file88.close()
//
//  val file89 = new PrintWriter("TypingML4Result/4.txt")
//  file89.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file89.close()
//
//  val file810 = new PrintWriter("TypingML4Result/4.txt")
//  file810.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file810.close()
//
//  val file811 = new PrintWriter("TypingML4Result/4.txt")
//  file811.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file811.close()
//
//  val file812 = new PrintWriter("TypingML4Result/4.txt")
//  file812.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file812.close()
//
//  val file813 = new PrintWriter("TypingML4Result/4.txt")
//  file813.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file813.close()
//
//  val file814 = new PrintWriter("TypingML4Result/4.txt")
//  file814.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file814.close()
//
//  val file815 = new PrintWriter("TypingML4Result/4.txt")
//  file815.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file815.close()
//
//  val file816 = new PrintWriter("TypingML4Result/4.txt")
//  file816.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file816.close()
//
//  val file817 = new PrintWriter("TypingML4Result/4.txt")
//  file817.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file817.close()
//
//  val file818 = new PrintWriter("TypingML4Result/4.txt")
//  file818.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file818.close()
//
//  val file819 = new PrintWriter("TypingML4Result/4.txt")
//  file819.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file819.close()
//
//  val file820 = new PrintWriter("TypingML4Result/4.txt")
//  file820.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file820.close()
//
//  val file821 = new PrintWriter("TypingML4Result/4.txt")
//  file821.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file821.close()
//
//  val file822 = new PrintWriter("TypingML4Result/4.txt")
//  file822.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file822.close()
//
//  val file823 = new PrintWriter("TypingML4Result/4.txt")
//  file823.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file823.close()
//
//  val file824 = new PrintWriter("TypingML4Result/4.txt")
//  file824.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file824.close()
//
//  val file825 = new PrintWriter("TypingML4Result/4.txt")
//  file825.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file825.close()
//
//  val file826 = new PrintWriter("TypingML4Result/4.txt")
//  file826.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file826.close()
//
//  val file827 = new PrintWriter("TypingML4Result/4.txt")
//  file827.write(typeSolve("let x = 3 < 2 in let y = 5 in if x then y else 2"))
//  file827.close()

}
