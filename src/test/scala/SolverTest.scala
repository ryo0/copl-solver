import org.scalatest.FunSuite
import parser.ast.IntVal
import solver.removeEnv

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
}
