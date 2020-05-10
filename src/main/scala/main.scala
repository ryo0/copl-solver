import solver.solver.solve
import java.io.PrintWriter

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
}
