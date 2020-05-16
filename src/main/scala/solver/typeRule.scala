package solver

import parser.ast._

object typeRule {
  sealed class MLType
  object MLIntType extends MLType
  object MLBoolType extends MLType
  case class MLFunType(arg: MLType, body: MLType) extends MLType
  case class MLListType(lst: MLType) extends MLType
  type TypeEnv = List[(String, MLType)]

  def getValFromTypeEnv(key: String, env: TypeEnv): MLType = {
    for (e <- env) {
      if (e._1 == key) {
        return e._2
      }
    }
    throw new Exception("keyがenvにありません")
  }

  sealed class TypeRule {
    def mlType: MLType = {
      this match {
        case TInt(_, _)                 => MLIntType
        case TBool(_, _)                => MLBoolType
        case TIf(_, _, _, _, _, _, tr3) => tr3.mlType
        case TVar(typeEnv, x)           => getValFromTypeEnv(x.name, typeEnv)
        case TLet(_, _, _, _, _, tr2) =>
          tr2.mlType
      }
    }
  }
  case class TInt(typeEnv: TypeEnv, i: IntVal) extends TypeRule
  case class TBool(typeEnv: TypeEnv, b: BoolVal) extends TypeRule
  case class TIf(typeEnv: TypeEnv,
                 e1: Exp,
                 e2: Exp,
                 e3: Exp,
                 tr1: TypeRule,
                 tr2: TypeRule,
                 tr3: TypeRule)
      extends TypeRule
  case class TVar(typeEnv: TypeEnv, x: Var) extends TypeRule
  case class TLet(typeEnv: TypeEnv,
                  x: Var,
                  e1: Exp,
                  e2: Exp,
                  tr1: TypeRule,
                  tr2: TypeRule)
      extends TypeRule
}
