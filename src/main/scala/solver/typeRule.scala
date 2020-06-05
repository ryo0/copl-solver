package solver

import parser.ast._

object typeRule {
  type Equations = List[Equation]
  case class Equation(left: MLType, right: MLType)
  type TypeAnswer = Map[MLType, MLType]

  implicit class Unification(E: Equations) {
    def substitute(typeAnswer: TypeAnswer): Equations = {
      E match {
        case Equation(left, right) :: rest =>
          Equation(left.substitute(typeAnswer), right.substitute(typeAnswer)) :: rest
            .substitute(typeAnswer)
        case List() =>
          List()
      }
    }
    def unify(): TypeAnswer = {
      E.distinct match {
        case List() =>
          Map()
        case Equation(MLFunType(t11, t12), MLFunType(t21, t22)) :: eqs =>
          (Equation(t11, t21) :: Equation(t12, t22) :: eqs).unify()
        case Equation(MLListType(t1), MLListType(t2)) :: eqs =>
          (Equation(t1, t2) :: eqs).unify()
        case Equation(left, right) :: eqs =>
          if (left == right) {
            eqs.unify()
          } else {
            (left, right) match {
              case (TypeVar(n), right) =>
                val ta: TypeAnswer = Map(TypeVar(n) -> right)
                val s = eqs.substitute(ta).unify()
                s + (TypeVar(n) -> right.substitute(s))
              case (left, TypeVar(n)) =>
                val ta: TypeAnswer = Map(TypeVar(n) -> left)
                val s = eqs.substitute(ta).unify()
                s + (TypeVar(n) -> left.substitute(s))
            }
          }
      }
    }
  }
  sealed class MLType {
    def string(typeEnv: TypeEnv = List()): String = {
      this match {
        case MLIntType  => "int"
        case MLBoolType => "bool"
        case MLFunType(arg, body) =>
          s"${arg.string(typeEnv)} -> (${body.string(typeEnv)})"
        case MLListType(lst) => s"${lst.string(typeEnv)} list"
        case TypeVar(name)   => s"$name"
      }
    }

    def substitute(typeAnswer: TypeAnswer): MLType = {
      this match {
        case TypeVar(n) => {
          val result = typeAnswer.get(TypeVar(n))
          result match {
            case Some(r) =>
              r
            case None =>
              TypeVar(n)
          }
        }
        case MLIntType  => MLIntType
        case MLBoolType => MLBoolType
        case MLFunType(arg, body) =>
          MLFunType(arg.substitute(typeAnswer), body.substitute(typeAnswer))
        case MLListType(lst) => MLListType(lst.substitute(typeAnswer))
      }
    }

    def ftv(a: TypeVar): Boolean = {
      this match {
        case TypeVar(_)           => true
        case MLIntType            => false
        case MLBoolType           => false
        case MLFunType(arg, body) => arg.ftv(a) || body.ftv(a)
        case MLListType(lst)      => lst.ftv(a)
      }
    }
  }
  object MLIntType extends MLType
  object MLBoolType extends MLType
  case class MLFunType(arg: MLType, body: MLType) extends MLType
  case class MLListType(lst: MLType) extends MLType
  case class TypeVar(name: String) extends MLType
  type TypeEnv = List[(String, MLType)]
  implicit class TypeEnvString(typeEnv: TypeEnv) {
    def string: String = {
      typeEnv.reverse
        .map(e => s"${e._1} : ${e._2.string(typeEnv)},")
        .mkString
        .dropRight(1)
    }
  }

  def getTypeFromTypeEnv(key: String, env: TypeEnv): MLType = {
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
        case TVar(typeEnv, x)           => x.getType(typeEnv)
        case TLet(_, _, _, _, _, tr2) =>
          tr2.mlType
        case TPlus(_, _, _, _, _)        => MLIntType
        case TMinus(_, _, _, _, _)       => MLIntType
        case TTimes(_, _, _, _, _)       => MLIntType
        case TLt(_, _, _, _, _)          => MLBoolType
        case TFun(typeEnv, x, e, tr1, t) => t
        case TApp(typeEnv, e1, e2, tr1, tr2) => {
          tr2.mlType
        }
        case TLetRec(typeEnv, x, y, e1, e2, tr1, tr2) => tr2.mlType
        case TNil(typeEnv, t)                         => t
        case TCons(typeEnv, e1, e2, tr1, tr2)         => tr2.mlType
        case TMatch(typeEnv, e1, e2, x, y, e3, tr1, tr2, tr3) =>
          tr3.mlType
      }
    }

    def getTypeEnv: TypeEnv = {
      this match {
        case TInt(typeEnv, _)                                 => typeEnv
        case TBool(typeEnv, _)                                => typeEnv
        case TIf(typeEnv, _, _, _, _, _, _)                   => typeEnv
        case TVar(typeEnv, _)                                 => typeEnv
        case TLet(typeEnv, _, _, _, _, _)                     => typeEnv
        case TPlus(typeEnv, _, _, _, _)                       => typeEnv
        case TMinus(typeEnv, _, _, _, _)                      => typeEnv
        case TTimes(typeEnv, _, _, _, _)                      => typeEnv
        case TLt(typeEnv, _, _, _, _)                         => typeEnv
        case TFun(typeEnv, _, _, _, _)                        => typeEnv
        case TApp(typeEnv, e1, e2, tr1, tr2)                  => typeEnv
        case TLetRec(typeEnv, x, y, e1, e2, tr1, tr2)         => typeEnv
        case TNil(typeEnv, t)                                 => typeEnv
        case TCons(typeEnv, e1, e2, tr1, tr2)                 => typeEnv
        case TMatch(typeEnv, e1, e2, x, y, e3, tr1, tr2, tr3) => typeEnv
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

  case class TPlus(typeEnv: TypeEnv,
                   e1: Exp,
                   e2: Exp,
                   tr1: TypeRule,
                   tr2: TypeRule)
      extends TypeRule

  case class TMinus(typeEnv: TypeEnv,
                    e1: Exp,
                    e2: Exp,
                    tr1: TypeRule,
                    tr2: TypeRule)
      extends TypeRule

  case class TTimes(typeEnv: TypeEnv,
                    e1: Exp,
                    e2: Exp,
                    tr1: TypeRule,
                    tr2: TypeRule)
      extends TypeRule

  case class TLt(typeEnv: TypeEnv,
                 e1: Exp,
                 e2: Exp,
                 tr1: TypeRule,
                 tr2: TypeRule)
      extends TypeRule

  case class TVar(typeEnv: TypeEnv, x: Var) extends TypeRule

  case class TLet(typeEnv: TypeEnv,
                  x: Var,
                  e1: Exp,
                  e2: Exp,
                  tr1: TypeRule,
                  tr2: TypeRule)
      extends TypeRule

  case class TFun(typeEnv: TypeEnv, x: Var, e: Exp, tr1: TypeRule, t: MLType)
      extends TypeRule

  case class TApp(typeEnv: TypeEnv,
                  e1: Exp,
                  e2: Exp,
                  tr1: TypeRule,
                  tr2: TypeRule)
      extends TypeRule

  case class TLetRec(typeEnv: TypeEnv,
                     x: Var,
                     y: Var,
                     e1: Exp,
                     e2: Exp,
                     tr1: TypeRule,
                     tr2: TypeRule)
      extends TypeRule

  case class TNil(typeEnv: TypeEnv, t: MLType) extends TypeRule

  case class TCons(typeEnv: TypeEnv,
                   e1: Exp,
                   e2: Exp,
                   tr1: TypeRule,
                   tr2: TypeRule)
      extends TypeRule

  case class TMatch(typeEnv: TypeEnv,
                    e1: Exp,
                    e2: Exp,
                    x: Var,
                    y: Var,
                    e3: Exp,
                    tr1: TypeRule,
                    tr2: TypeRule,
                    tr3: TypeRule)
      extends TypeRule

  implicit class NestString(str: String) {
    def mul(nest: Int): String = {
      if (nest == 0) { "" } else if (nest == 1) {
        str
      } else {
        str + str.mul(nest - 1)
      }
    }
  }
  implicit class TypeRuleString(typeRule: TypeRule) {
    def string(nest: Int = 0): String = {
      val indent = "     ".mul(nest)
      val indentPlus1 = "     ".mul(nest + 1)
      typeRule match {
        case TInt(typeEnv, i) =>
          s"${typeEnv.string} |- ${i.string} : int by T-Int{};"
        case TBool(typeEnv, b) =>
          s"${typeEnv.string} |- ${b.string} : bool by T-Bool{};"
        case TPlus(typeEnv, e1, e2, tr1, tr2) =>
          s"${typeEnv.string} |- ${e1.string} + ${e2.string} : int by T-Plus{\n" +
            s"$indentPlus1${tr1.string(nest + 1)}\n" +
            s"$indentPlus1${tr2.string(nest + 1)}\n" +
            s"$indent};"
        case TMinus(typeEnv, e1, e2, tr1, tr2) =>
          s"${typeEnv.string} |- ${e1.string} - ${e2.string} : int by T-Minus{\n" +
            s"$indentPlus1${tr1.string(nest + 1)}\n" +
            s"$indentPlus1${tr2.string(nest + 1)}\n" +
            s"$indent};"
        case TTimes(typeEnv, e1, e2, tr1, tr2) =>
          s"${typeEnv.string} |- ${e1.string} * ${e2.string} : int by T-Times{\n" +
            s"$indentPlus1${tr1.string(nest + 1)}\n" +
            s"$indentPlus1${tr2.string(nest + 1)}\n" +
            s"$indent};"
        case TLt(typeEnv, e1, e2, tr1, tr2) =>
          s"${typeEnv.string} |- ${e1.string} < ${e2.string} : bool by T-Lt{\n" +
            s"$indentPlus1${tr1.string(nest + 1)}\n" +
            s"$indentPlus1${tr2.string(nest + 1)}\n" +
            s"$indent};"
        case TIf(typeEnv, e1, e2, e3, tr1, tr2, tr3) =>
          s"${typeEnv.string} |- if ${e1.string} then ${e2.string} else ${e3.string}: ${this.typeRule.mlType
            .string(typeEnv)} by T-If{\n" +
            s"$indentPlus1${tr1.string(nest + 1)}\n" +
            s"$indentPlus1${tr2.string(nest + 1)}\n" +
            s"$indentPlus1${tr3.string(nest + 1)}\n" +
            s"$indent};"
        case TVar(typeEnv, x) =>
          s"${typeEnv.string} |- ${x.string} : ${getTypeFromTypeEnv(x.name, typeEnv).string(typeEnv)} by T-Var{};"
        case TLet(typeEnv, x, e1, e2, tr1, tr2) =>
          s"${typeEnv.string} |- let ${x.string} = ${e1.string} in ${e2.string} : ${this.typeRule.mlType
            .string(typeEnv)} by T-Let{\n" +
            s"$indentPlus1${tr1.string(nest + 1)}\n" +
            s"$indentPlus1${tr2.string(nest + 1)}\n" +
            s"$indent};"
        case TFun(typeEnv, x, e, tr1, t) =>
          s"${typeEnv.string} |-fun ${x.string} -> ${e.string} : ${t.string(typeEnv)} by T-Fun{\n" +
            s"$indentPlus1${tr1.string(nest + 1)}\n" +
            s"$indent};"
        case TApp(typeEnv, e1, e2, tr1, tr2) =>
          s"${typeEnv.string} |- ${e1.string} ${e2.string} : ${this.typeRule.mlType
            .string(typeEnv)} by T-App{\n" +
            s"$indentPlus1${tr1.string(nest + 1)}\n" +
            s"$indentPlus1${tr2.string(nest + 1)}\n" +
            s"$indent};"
        case TLetRec(typeEnv, x, y, e1, e2, tr1, tr2) =>
          s"${typeEnv.string} |- let rec ${x.string} = fun ${y.string} -> ${e1.string} in ${e2.string} : ${this.typeRule.mlType
            .string(typeEnv)} by T-LetRec{\n" +
            s"$indentPlus1${tr1.string(nest + 1)}\n" +
            s"$indentPlus1${tr2.string(nest + 1)}\n" +
            s"$indent};"
        case TCons(typeEnv, e1, e2, tr1, tr2) =>
          s"${typeEnv.string} |- ${e1.string} :: ${e2.string} : ${this.typeRule.mlType
            .string(typeEnv)} by T-Cons{\n" +
            s"$indentPlus1${tr1.string(nest + 1)}\n" +
            s"$indentPlus1${tr2.string(nest + 1)}\n" +
            s"$indent};"
        case TMatch(typeEnv, e1, e2, x, y, e3, tr1, tr2, tr3) =>
          s"${typeEnv.string} |- match ${e1.string} with [] -> ${e2.string} | ${x.string} :: ${y.string} -> ${e3.string} : ${this.typeRule.mlType
            .string(typeEnv)} by T-Cons{\n" +
            s"$indentPlus1${tr1.string(nest + 1)}\n" +
            s"$indentPlus1${tr2.string(nest + 1)}\n" +
            s"$indentPlus1${tr3.string(nest + 1)}\n" +
            s"$indent};"
      }
    }
  }
}
