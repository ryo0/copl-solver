package solver

import parser.ast._

object typeRule {
  type Equations = List[Equation]
  case class Equation(left: MLType, right: MLType)
  type TypeAnswer = List[(MLType, MLType)]

  def getTypeFromTypeAnswer(key: String,
                            typeAnswer: TypeAnswer): Option[MLType] = {
    for (e <- typeAnswer) {
      e._1 match {
        case typeVar: TypeVar =>
          if (key == typeVar.name) {
            return Some(e._2)
          }
        case _ =>
      }
    }
    None
  }

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
          List()
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
                val ta: TypeAnswer = List((TypeVar(n), right))
                val s = eqs.substitute(ta).unify()
                s :+ (TypeVar(n), right.substitute(s))
              case (left, TypeVar(n)) =>
                val ta: TypeAnswer = List((TypeVar(n), left))
                val s = eqs.substitute(ta).unify()
                s :+ (TypeVar(n) -> left.substitute(s))
            }
          }
      }
    }
  }
  def typeVarToString(name: String, typeEnv: TypeEnv): String = {
    val result = getTypeFromTypeEnv(name, typeEnv)
    result match {
      case Some(r) =>
        r.string()
      case _ =>
        name
    }
  }
  sealed class MLType {
    def string(typeEnv: TypeEnv = List()): String = {
      this match {
        case MLIntType  => "int"
        case MLBoolType => "bool"
        case MLFunType(arg, body) =>
          s"(${arg.string()} -> ${body.string()})"
        case MLListType(lst) => s"${lst.string()} list"
        case TypeVar(name) => {
          typeVarToString(name, typeEnv)
        }
      }
    }
    def fillTypeVar(): MLType = {
      this match {
        case TypeVar(n) => {
          MLIntType
        }
        case MLIntType  => MLIntType
        case MLBoolType => MLBoolType
        case MLFunType(arg, body) =>
          MLFunType(arg.fillTypeVar(), body.fillTypeVar())
        case MLListType(lst) => MLListType(lst.fillTypeVar())
      }
    }

    def substitute(typeAnswer: TypeAnswer): MLType = {
      this match {
        case TypeVar(n) => {
          val result = getTypeFromTypeAnswer(n, typeAnswer)
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

    def substitute(typeAnswer: TypeAnswer): TypeEnv = {
      typeEnv.map(e => (e._1, e._2.substitute(typeAnswer)))
    }

    def fillTypeVar(): TypeEnv = {
      typeEnv.map(e => (e._1, e._2.fillTypeVar()))
    }
  }

  def getTypeFromTypeEnv(key: String, env: TypeEnv): Option[MLType] = {
    for (e <- env) {
      if (e._1 == key) {
        return Some(e._2)
      }
    }
    None
  }

  def removeDuplicationOfTypeAnswer(typeAnswer: TypeAnswer): TypeAnswer = {
    var result: List[(MLType, MLType)] = List()
    var keys: Set[MLType] = Set()
    typeAnswer.foreach { a =>
      if (keys.contains(a._1)) {
        if (!a._2.isInstanceOf[TypeVar]) {
          result = result.filter(r => r._1 != a._1) :+ (a._1, a._2)
        }
      } else {
        if (!result.map(r => r._1).contains(a._1)) {
          result = result :+ (a._1, a._2)
        }
        keys = keys + a._1
      }
    }
    result
  }

  def normalize(answer: TypeAnswer): TypeAnswer = {
    removeDuplicationOfTypeAnswer(answer.map {
      case (TypeVar(n), other) =>
        getAnswerRec(n, answer) match {
          case Some(r) =>
            (TypeVar(n), r)
          case None =>
            (TypeVar(n), other)
        }
      case (a, b) =>
        (a, b)
    })
  }

  def removeFromAnswer(n: String, m: String, answer: TypeAnswer): TypeAnswer = {
    answer.filter(a => !(a._1 == TypeVar(n) && a._2 == TypeVar(m)))
  }

  def getAnswerRec(n: String, answer: TypeAnswer): Option[MLType] = {
//    input例
//    n = 158
//    answer = List(
//      (TypeVar("x158"), TypeVar("x180")),
//      (TypeVar("a"), MLIntType),
//      (TypeVar("x180"), TypeVar("x158")),
//      (MLBoolType, MLBoolType),
//      (TypeVar("x172"), TypeVar("x158")),
//      (TypeVar("x158"), TypeVar("x172")),
//      (TypeVar("x158"), TypeVar("x158")),
//      (TypeVar("x"), MLBoolType),
//      (TypeVar("x158"), TypeVar("x158")),
//      (TypeVar("x144"), TypeVar("x144")),
//      (TypeVar("x172"), MLFunType(MLIntType, MLIntType)),
//      (TypeVar("x"), TypeVar("z")),
//      (MLFunType(MLIntType, MLIntType), TypeVar("'x")),
//      (TypeVar("x144"), TypeVar("x166")),
//      (TypeVar("x166"), TypeVar("x144")))
//     output = MLFunType(MLIntType, MLIntType)
//    158 -> 180を取得
//    再帰呼び出し
//    180 -> 158を取得
//    158, 158なので1つ消して180でリトライ
//    180に158以外が出てくるか→
//    TypeVarかつ158以外が出てきたよ→今度はそいつをキーにしてリトライ
//    TypeVarじゃないのが出てきたよ→それが答え
//    158 -> 158
//    158, 158なので消してリトライ
//    158 -> 172
//    172キーにしてリトライして158以外が出てくるか→
//    TypeVarかつ158以外が出てきたよ→今度はそいつをキーにしてリトライ
//    TypeVarじゃないのが出てきたよ→それが答え
//    158 -> None -> 全体Noneで終わり
    getTypeFromTypeAnswer(n, answer) match {
      case Some(TypeVar(m)) =>
        val ans2 = removeFromAnswer(n, m, answer)
        if (n == m) {
          getAnswerRec(n, ans2)
        } else {
          getAnswerRec(m, ans2) match {
            case Some(TypeVar(l)) =>
              val ans3 = removeFromAnswer(m, l, ans2)
              if (n == l) {
                getAnswerRec(m, ans3)
              } else {
                getAnswerRec(l, ans3)
              }
            case Some(r) =>
              Some(r)
            case None =>
              getAnswerRec(n, ans2)
          }
        }
      case other =>
        other
    }
  }

  def fixTypeAnswer(typeAnswer: TypeAnswer): TypeAnswer = {
    var result: TypeAnswer = List()
    typeAnswer.foreach {
      case (MLFunType(a1, b1), MLFunType(a2, b2)) =>
        result = fixTypeAnswer(List((a1, a2), (b1, b2))) ::: result
      case (MLListType(lst1), MLListType(lst2)) =>
        result = fixTypeAnswer(List((lst1, lst2))) ::: result
      case (first1, first2) =>
        result = List((first1, first2)) ::: result
      case _ =>
        ()
    }
    result
  }

  def getTypeAnswer(typeRule: TypeRule): TypeAnswer = {
    println("unfixed:", getTypeAnswerOfTypeVars(typeRule))

    println("fixed:", fixTypeAnswer(getTypeAnswerOfTypeVars(typeRule)))
    println(
      "normalized:",
      normalize(fixTypeAnswer(getTypeAnswerOfTypeVars(typeRule)))
    )
    println("unnormalized:", fixTypeAnswer(getTypeAnswerOfTypeVars(typeRule)))
    normalize(fixTypeAnswer(getTypeAnswerOfTypeVars(typeRule)))
  }

  def getTypeAnswerOfTypeVars(typeRule: TypeRule): TypeAnswer = {
    typeRule match {
      case TInt(typeEnv, i) =>
        List()
      case TBool(typeEnv, b) =>
        List()
      case TPlus(typeEnv, e1, e2, tr1, tr2) =>
        getTypeAnswerOfTypeVars(tr1) ::: getTypeAnswerOfTypeVars(tr2)
      case TMinus(typeEnv, e1, e2, tr1, tr2) =>
        getTypeAnswerOfTypeVars(tr1) ::: getTypeAnswerOfTypeVars(tr2)
      case TTimes(typeEnv, e1, e2, tr1, tr2) =>
        getTypeAnswerOfTypeVars(tr1) ::: getTypeAnswerOfTypeVars(tr2)
      case TLt(typeEnv, e1, e2, tr1, tr2) =>
        getTypeAnswerOfTypeVars(tr1) ::: getTypeAnswerOfTypeVars(tr2)
      case TIf(typeEnv, e1, e2, e3, tr1, tr2, tr3, t) =>
        getTypeAnswerOfTypeVars(tr1) ::: getTypeAnswerOfTypeVars(tr2) ::: getTypeAnswerOfTypeVars(
          tr3
        )
      case TVar(typeEnv, x, t) =>
        List()
      case TLet(typeEnv, x, e1, e2, tr1, tr2, t) =>
        (tr2.mlType, t) :: (t, tr2.mlType) :: getTypeAnswerOfTypeVars(tr1) ::: getTypeAnswerOfTypeVars(
          tr2
        )
      case TFun(typeEnv, x, e, tr1, t) =>
        val myType = t.asInstanceOf[MLFunType]
        getTypeAnswerOfTypeVars(tr1) :+ (tr1.mlType, myType.body) :+ (myType.body, tr1.mlType)
      case TApp(typeEnv, e1, e2, tr1, tr2, t) =>
        val funType = tr1.mlType.asInstanceOf[MLFunType]
        val argTypeVar = funType.arg
        val argType = tr2.mlType
        val bodyTypeVar = funType.body
        val bodyType = t
        println(
          (argTypeVar, argType),
          (bodyTypeVar, bodyType),
          (argType, argTypeVar),
          (bodyType, bodyTypeVar)
        )
        List(
          (argTypeVar, argType),
          (bodyTypeVar, bodyType),
          (argType, argTypeVar),
          (bodyType, bodyTypeVar)
        ) ::: getTypeAnswerOfTypeVars(tr1) ::: getTypeAnswerOfTypeVars(tr2)
      case TLetRec(typeEnv, x, y, e1, e2, tr1, tr2, t) =>
        getTypeAnswerOfTypeVars(tr1) ::: getTypeAnswerOfTypeVars(tr2)
      case TCons(typeEnv, e1, e2, tr1, tr2, t) =>
        getTypeAnswerOfTypeVars(tr1) ::: getTypeAnswerOfTypeVars(tr2)
      case TNil(typeEnv, t) =>
        List()
      case TMatch(typeEnv, e1, e2, x, y, e3, tr1, tr2, tr3, t) =>
        getTypeAnswerOfTypeVars(tr1) ::: getTypeAnswerOfTypeVars(tr2) ::: getTypeAnswerOfTypeVars(
          tr3
        )
    }
  }

  sealed class TypeRule {
    def mlType: MLType = {
      this match {
        case TInt(_, _)                  => MLIntType
        case TBool(_, _)                 => MLBoolType
        case TIf(_, _, _, _, _, _, _, t) => t
        case TVar(typeEnv, x, t)         => t
        case TLet(_, _, _, _, _, _, t) =>
          t
        case TPlus(_, _, _, _, _)                        => MLIntType
        case TMinus(_, _, _, _, _)                       => MLIntType
        case TTimes(_, _, _, _, _)                       => MLIntType
        case TLt(_, _, _, _, _)                          => MLBoolType
        case TFun(typeEnv, x, e, tr1, t)                 => t
        case TApp(typeEnv, e1, e2, tr1, tr2, t)          => t
        case TLetRec(typeEnv, x, y, e1, e2, tr1, tr2, t) => t
        case TNil(typeEnv, t)                            => t
        case TCons(typeEnv, e1, e2, tr1, tr2, t)         => t
        case TMatch(typeEnv, e1, e2, x, y, e3, tr1, tr2, tr3, t) =>
          t
      }
    }
    def substitute(typeAnswer: TypeAnswer): TypeRule = {
      this match {
        case TInt(typeEnv, i) =>
          TInt(typeEnv.substitute(typeAnswer), i)
        case TBool(typeEnv, b) =>
          TBool(typeEnv.substitute(typeAnswer), b)
        case TPlus(typeEnv, e1, e2, tr1, tr2) =>
          TPlus(
            typeEnv.substitute(typeAnswer),
            e1,
            e2,
            tr1.substitute(typeAnswer),
            tr2.substitute(typeAnswer)
          )
        case TMinus(typeEnv, e1, e2, tr1, tr2) =>
          TMinus(
            typeEnv.substitute(typeAnswer),
            e1,
            e2,
            tr1.substitute(typeAnswer),
            tr2.substitute(typeAnswer)
          )
        case TTimes(typeEnv, e1, e2, tr1, tr2) =>
          TTimes(
            typeEnv.substitute(typeAnswer),
            e1,
            e2,
            tr1.substitute(typeAnswer),
            tr2.substitute(typeAnswer)
          )
        case TLt(typeEnv, e1, e2, tr1, tr2) =>
          TLt(
            typeEnv.substitute(typeAnswer),
            e1,
            e2,
            tr1.substitute(typeAnswer),
            tr2.substitute(typeAnswer)
          )
        case TIf(typeEnv, e1, e2, e3, tr1, tr2, tr3, t) =>
          TIf(
            typeEnv.substitute(typeAnswer),
            e1,
            e2,
            e3,
            tr1.substitute(typeAnswer),
            tr2.substitute(typeAnswer),
            tr3.substitute(typeAnswer),
            t.substitute(typeAnswer)
          )
        case TVar(typeEnv, x, t) =>
          TVar(typeEnv.substitute(typeAnswer), x, t.substitute(typeAnswer))
        case TLet(typeEnv, x, e1, e2, tr1, tr2, t) =>
          TLet(
            typeEnv.substitute(typeAnswer),
            x,
            e1,
            e2,
            tr1.substitute(typeAnswer),
            tr2.substitute(typeAnswer),
            t.substitute(typeAnswer)
          )
        case TFun(typeEnv, x, e, tr1, t) =>
          TFun(
            typeEnv.substitute(typeAnswer),
            x,
            e,
            tr1.substitute(typeAnswer),
            t.substitute(typeAnswer).asInstanceOf[MLFunType]
          )
        case TApp(typeEnv, e1, e2, tr1, tr2, t) =>
          TApp(
            typeEnv.substitute(typeAnswer),
            e1,
            e2,
            tr1.substitute(typeAnswer),
            tr2.substitute(typeAnswer),
            t.substitute(typeAnswer)
          )
        case TApp(typeEnv, e1, e2, tr1, tr2, t) =>
          TApp(
            typeEnv.substitute(typeAnswer),
            e1,
            e2,
            tr1.substitute(typeAnswer),
            tr2.substitute(typeAnswer),
            t.substitute(typeAnswer)
          )
        case TLetRec(typeEnv, x, y, e1, e2, tr1, tr2, t) =>
          TLetRec(
            typeEnv.substitute(typeAnswer),
            x,
            y,
            e1,
            e2,
            tr1.substitute(typeAnswer),
            tr2.substitute(typeAnswer),
            t.substitute(typeAnswer)
          )
        case TCons(typeEnv, e1, e2, tr1, tr2, t) =>
          TCons(
            typeEnv.substitute(typeAnswer),
            e1,
            e2,
            tr1.substitute(typeAnswer),
            tr2.substitute(typeAnswer),
            t.substitute(typeAnswer)
          )
        case TNil(typeEnv, t) =>
          TNil(typeEnv.substitute(typeAnswer), t.substitute(typeAnswer))
        case TMatch(typeEnv, e1, e2, x, y, e3, tr1, tr2, tr3, t) =>
          TMatch(
            typeEnv.substitute(typeAnswer),
            e1,
            e2,
            x,
            y,
            e3,
            tr1.substitute(typeAnswer),
            tr2.substitute(typeAnswer),
            tr3.substitute(typeAnswer),
            t.substitute(typeAnswer)
          )
      }
    }
    def fillTypeVar(): TypeRule = {
      this match {
        case TInt(typeEnv, i) =>
          TInt(typeEnv.fillTypeVar(), i)
        case TBool(typeEnv, b) =>
          TBool(typeEnv.fillTypeVar(), b)
        case TPlus(typeEnv, e1, e2, tr1, tr2) =>
          TPlus(
            typeEnv.fillTypeVar(),
            e1,
            e2,
            tr1.fillTypeVar(),
            tr2.fillTypeVar()
          )
        case TMinus(typeEnv, e1, e2, tr1, tr2) =>
          TMinus(
            typeEnv.fillTypeVar(),
            e1,
            e2,
            tr1.fillTypeVar(),
            tr2.fillTypeVar()
          )
        case TTimes(typeEnv, e1, e2, tr1, tr2) =>
          TTimes(
            typeEnv.fillTypeVar(),
            e1,
            e2,
            tr1.fillTypeVar(),
            tr2.fillTypeVar()
          )
        case TLt(typeEnv, e1, e2, tr1, tr2) =>
          TLt(
            typeEnv.fillTypeVar(),
            e1,
            e2,
            tr1.fillTypeVar(),
            tr2.fillTypeVar()
          )
        case TIf(typeEnv, e1, e2, e3, tr1, tr2, tr3, t) =>
          TIf(
            typeEnv.fillTypeVar(),
            e1,
            e2,
            e3,
            tr1.fillTypeVar(),
            tr2.fillTypeVar(),
            tr3.fillTypeVar(),
            t.fillTypeVar()
          )
        case TVar(typeEnv, x, t) =>
          TVar(typeEnv.fillTypeVar(), x, t.fillTypeVar())
        case TLet(typeEnv, x, e1, e2, tr1, tr2, t) =>
          TLet(
            typeEnv.fillTypeVar(),
            x,
            e1,
            e2,
            tr1.fillTypeVar(),
            tr2.fillTypeVar(),
            t.fillTypeVar()
          )
        case TFun(typeEnv, x, e, tr1, t) =>
          TFun(
            typeEnv.fillTypeVar(),
            x,
            e,
            tr1.fillTypeVar(),
            t.fillTypeVar().asInstanceOf[MLFunType]
          )
        case TApp(typeEnv, e1, e2, tr1, tr2, t) =>
          TApp(
            typeEnv.fillTypeVar(),
            e1,
            e2,
            tr1.fillTypeVar(),
            tr2.fillTypeVar(),
            t.fillTypeVar()
          )
        case _ =>
          this
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
                 tr3: TypeRule,
                 t: MLType)
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

  case class TVar(typeEnv: TypeEnv, x: Var, t: MLType) extends TypeRule

  case class TLet(typeEnv: TypeEnv,
                  x: Var,
                  e1: Exp,
                  e2: Exp,
                  tr1: TypeRule,
                  tr2: TypeRule,
                  t: MLType)
      extends TypeRule

  case class TFun(typeEnv: TypeEnv, x: Var, e: Exp, tr1: TypeRule, t: MLFunType)
      extends TypeRule

  case class TApp(typeEnv: TypeEnv,
                  e1: Exp,
                  e2: Exp,
                  tr1: TypeRule,
                  tr2: TypeRule,
                  t: MLType)
      extends TypeRule

  case class TLetRec(typeEnv: TypeEnv,
                     x: Var,
                     y: Var,
                     e1: Exp,
                     e2: Exp,
                     tr1: TypeRule,
                     tr2: TypeRule,
                     t: MLType)
      extends TypeRule

  case class TNil(typeEnv: TypeEnv, t: MLType) extends TypeRule

  case class TCons(typeEnv: TypeEnv,
                   e1: Exp,
                   e2: Exp,
                   tr1: TypeRule,
                   tr2: TypeRule,
                   t: MLType)
      extends TypeRule

  case class TMatch(typeEnv: TypeEnv,
                    e1: Exp,
                    e2: Exp,
                    x: Var,
                    y: Var,
                    e3: Exp,
                    tr1: TypeRule,
                    tr2: TypeRule,
                    tr3: TypeRule,
                    t: MLType)
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
        case TIf(typeEnv, e1, e2, e3, tr1, tr2, tr3, t) =>
          s"${typeEnv.string} |- if ${e1.string} then ${e2.string} else ${e3.string}: ${t
            .string(typeEnv)} by T-If{\n" +
            s"$indentPlus1${tr1.string(nest + 1)}\n" +
            s"$indentPlus1${tr2.string(nest + 1)}\n" +
            s"$indentPlus1${tr3.string(nest + 1)}\n" +
            s"$indent};"
        case TVar(typeEnv, x, t) =>
          s"${typeEnv.string} |- ${x.string} : ${t.string(typeEnv)} by T-Var{};"
        case TLet(typeEnv, x, e1, e2, tr1, tr2, t) =>
          s"${typeEnv.string} |- let ${x.string} = ${e1.string} in ${e2.string} : ${t
            .string(typeEnv)} by T-Let{\n" +
            s"$indentPlus1${tr1.string(nest + 1)}\n" +
            s"$indentPlus1${tr2.string(nest + 1)}\n" +
            s"$indent};"
        case TFun(typeEnv, x, e, tr1, t) =>
          s"${typeEnv.string} |-fun ${x.string} -> ${e.string} :  ${t.string(typeEnv)}  by T-Fun{\n" +
            s"$indentPlus1${tr1.string(nest + 1)}\n" +
            s"$indent};"
        case TApp(typeEnv, e1, e2, tr1, tr2, t) =>
          s"${typeEnv.string} |- ${e1.string} ${e2.string} :  ${t.string(typeEnv)}  by T-App{\n" +
            s"$indentPlus1${tr1.string(nest + 1)}\n" +
            s"$indentPlus1${tr2.string(nest + 1)}\n" +
            s"$indent};"
        case TLetRec(typeEnv, x, y, e1, e2, tr1, tr2, t) =>
          s"${typeEnv.string} |- let rec ${x.string} = fun ${y.string} -> ${e1.string} in ${e2.string} :  ${t
            .string(typeEnv)}  by T-LetRec{\n" +
            s"$indentPlus1${tr1.string(nest + 1)}\n" +
            s"$indentPlus1${tr2.string(nest + 1)}\n" +
            s"$indent};"
        case TCons(typeEnv, e1, e2, tr1, tr2, t) =>
          s"${typeEnv.string} |- ${e1.string} :: ${e2.string} :  ${t.string(typeEnv)}  by T-Cons{\n" +
            s"$indentPlus1${tr1.string(nest + 1)}\n" +
            s"$indentPlus1${tr2.string(nest + 1)}\n" +
            s"$indent};"
        case TMatch(typeEnv, e1, e2, x, y, e3, tr1, tr2, tr3, t) =>
          s"${typeEnv.string} |- match ${e1.string} with [] -> ${e2.string} | ${x.string} :: ${y.string} -> ${e3.string} :  ${t
            .string(typeEnv)}  by T-Cons{\n" +
            s"$indentPlus1${tr1.string(nest + 1)}\n" +
            s"$indentPlus1${tr2.string(nest + 1)}\n" +
            s"$indentPlus1${tr3.string(nest + 1)}\n" +
            s"$indent};"
        case TNil(typeEnv, t) =>
          s"${typeEnv.string} |- [] :  ${t.string(typeEnv)}  by T-Nil{};"
      }
    }
  }
}
