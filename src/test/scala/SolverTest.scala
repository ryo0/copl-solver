import org.scalatest.FunSuite
import parser.ast.{Closure, FunCall, FunExp, InfixExp, IntVal, Plus, Var}
import solver.solver.removeEnv
import solver.solver.envToString
import solver.solver.funExpToStringWithEnv
import solver.solver.solveFunExp

class SolverTest extends FunSuite {
  test("removeEnv") {
    assert(
      removeEnv("x", List(("x", IntVal(1)), ("y", IntVal(2)))) === List(
        ("y", IntVal(2))
      )
    )
    assert(
      removeEnv("y", List(("x", IntVal(1)), ("y", IntVal(2)))) === List(
        ("x", IntVal(1))
      )
    )
    assert(
      removeEnv("y", List(("x", IntVal(1)), ("y", IntVal(2)), ("z", IntVal(3)))) === List(
        ("x", IntVal(1)),
        ("z", IntVal(3))
      )
    )
  }
  test("envToString") {
    assert(
      envToString(
        List(
          ("f", FunExp(Var("x"), Var("x"))),
          ("y", IntVal(1)),
          ("x", IntVal(2))
        )
      ) === "x = 2,y = 1,f = () [fun x -> x]"
    )
    assert(
      envToString(
        List(
          ("f", Closure(List(("z", IntVal(10))), FunExp(Var("x"), Var("x")))),
          ("y", IntVal(1)),
          ("x", IntVal(2))
        )
      ) === "x = 2,y = 1,f = (z = 10) [fun x -> x]"
    )
  }

  test("funExpToStringWithEnv") {
    assert(
      funExpToStringWithEnv(
        FunExp(Var("a"), FunCall(Var("f"), List(IntVal(1)))),
        List(
          ("f", Closure(List(("z", IntVal(10))), FunExp(Var("x"), Var("x")))),
          ("y", IntVal(1)),
          ("x", IntVal(2))
        )
      ) === "(x = 2,y = 1,f = (z = 10) [fun x -> x]) [fun a -> (f 1 )]"
    )
  }
  test("solveFunExp") {
    println(
      solveFunExp(
        FunExp(Var("x"), InfixExp(Var("x"), Plus, Var("y"))),
        List(IntVal(1)),
        List(("y", IntVal(5)))
      )
    )
  }
}
