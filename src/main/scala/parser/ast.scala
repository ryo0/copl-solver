package parser

import solver.rule._
import solver.typeRule._

object ast {
  def typeVarNameCounter(): () => Int = {
    var counter = 0
    def counterBody(): Int = {
      counter += 1
      counter
    }
    counterBody
  }
  def typeVarNameGenerator(): () => String = {
    val counter = typeVarNameCounter()
    def generatorBody(): String = {
      "'x" + counter()
    }
    generatorBody
  }
  val newTypeVarName: () => String = typeVarNameGenerator()
  val newTypeVar: () => TypeVar = () => {
    val r = TypeVar(newTypeVarName())
    r
  }
  sealed class Op
  object Plus extends Op
  object Minus extends Op
  object Asterisk extends Op
  object Slash extends Op
  object GreaterThan extends Op
  object LessThan extends Op

  object Cons extends Op
  sealed class Exp {
    def getTypeWithoutAnswer(typeEnv: TypeEnv = List()): MLType = {
      val e = this.typeExtract(typeEnv)
      val typeAnswerMap = e._1.unify()
      e._2.substitute(typeAnswerMap)
    }
    def getType(typeEnv: TypeEnv = List(), myEqAnswer: MLType): MLType = {
      val e = this.typeExtract(typeEnv)
      val typeAnswerMap =
        (Equation(myEqAnswer, e._2) :: e._1).unify()
      e._2.substitute(typeAnswerMap)
    }
    def typeExtract(typeEnv: TypeEnv): (Equations, MLType) = {
      this match {
        case IntVal(n) =>
          (List(), MLIntType)
        case BoolVal(b) =>
          (List(), MLBoolType)
        case Var(n) =>
          getTypeFromTypeEnv(n, typeEnv) match {
            case Some(r) => (List(), r)
            case _ =>
              throw new Exception("envにkeyがない")
          }
        case InfixExp(e1, op, e2) =>
          val (eq1, t1) = e1.typeExtract(typeEnv)
          val (eq2, t2) = e2.typeExtract(typeEnv)
          val eq3 = (eq1 ::: eq2) :+
            Equation(t1, MLIntType) :+ Equation(t2, MLIntType)
          op match {
            case LessThan =>
              (eq3, MLBoolType)
            case _ =>
              (eq3, MLIntType)
          }
        case IfExp(condExp, thenExp, elseExp) =>
          val (eq1, t1) = condExp.typeExtract(typeEnv)
          val (eq2, t2) = thenExp.typeExtract(typeEnv)
          val (eq3, t3) = elseExp.typeExtract(typeEnv)
          val eq4 = (eq1 ::: eq2 ::: eq3) :+
            Equation(t1, MLBoolType) :+ Equation(t2, t3)
          (eq4, t2)
        case LetExp(variable, valueExp, inExp) =>
          val (eq1, t1) = valueExp.typeExtract(typeEnv)
          val (eq2, t2) = inExp.typeExtract((variable.name, t1) :: typeEnv)
          (eq1 ::: eq2, t2)
        case FunExp(param, body) =>
          val a = newTypeVar()
          val (eq, t0) = body.typeExtract((param.name, a) :: typeEnv)
          (eq, MLFunType(a, t0))
        case FunCall(funName, arg) =>
          val (eq1, t1) = funName.typeExtract(typeEnv)
          val (eq2, t2) = arg.typeExtract(typeEnv)
          val a = newTypeVar()
          val eq3 = Equation(t1, MLFunType(t2, a)) :: eq1 ::: eq2
          (eq3, a)
        case LetRecExp(variable, RecFunExp(_, param, body), inExp) =>
          val a1 = newTypeVar()
          val a2 = newTypeVar()
          val (eq1, t1) =
            body.typeExtract((variable.name, a1) :: (param.name, a2) :: typeEnv)
          val (eq2, t2) = inExp.typeExtract((variable.name, a1) :: typeEnv)
          val eq3 = Equation(a1, MLFunType(a2, t1)) :: eq1 ::: eq2
          (eq3, t2)
        case EmptyList =>
          val a = newTypeVar()
          (List(), MLListType(a))
        case EList(left, right) =>
          val (eq1, t1) = left.typeExtract(typeEnv)
          val (eq2, t2) = right.typeExtract(typeEnv)
          val eq3 = Equation(t2, MLListType(t1)) :: (eq1 ::: eq2)
          (eq3, t2)
        case Match(
            e1: Var,
            Pattern(EmptyList, right1) :: Pattern(EList(Var(x), Var(y)), right2) :: List()
            ) =>
          val (eq1, t1) = e1.typeExtract(typeEnv)
          val (eq2, t2) = right1.typeExtract(typeEnv)
          val a = newTypeVar()
          val (eq3, t3) =
            right2.typeExtract((x, a) :: (y, MLListType(a)) :: typeEnv)
          val eq4 = Equation(t1, MLListType(a)) :: Equation(t2, t3) :: eq1 ::: eq2 ::: eq3
          (eq4, t2)
      }
    }
    def typeSolve(typeEnv: TypeEnv, eqAnswer: MLType): TypeRule = {
      val myType = eqAnswer match {
        case TypeVar(n) =>
          val result = getTypeFromTypeEnv(n, typeEnv)
          result match {
            case Some(r) =>
              r
            case _ =>
              TypeVar(n)
          }
        case a =>
          a
      }
      this match {
        case IntVal(n) =>
          TInt(typeEnv, IntVal(n))
        case BoolVal(b) =>
          TBool(typeEnv, BoolVal(b))
        case InfixExp(e1, Plus, e2) =>
          val tr1 = e1.typeSolve(typeEnv, MLIntType)
          val tr2 = e2.typeSolve(typeEnv, MLIntType)
          TPlus(typeEnv, e1, e2, tr1, tr2)
        case InfixExp(e1, Minus, e2) =>
          val tr1 = e1.typeSolve(typeEnv, MLIntType)
          val tr2 = e2.typeSolve(typeEnv, MLIntType)
          TMinus(typeEnv, e1, e2, tr1, tr2)
        case InfixExp(e1, Asterisk, e2) =>
          val tr1 = e1.typeSolve(typeEnv, MLIntType)
          val tr2 = e2.typeSolve(typeEnv, MLIntType)
          TTimes(typeEnv, e1, e2, tr1, tr2)
        case InfixExp(e1, LessThan, e2) =>
          val tr1 = e1.typeSolve(typeEnv, MLIntType)
          val tr2 = e2.typeSolve(typeEnv, MLIntType)
          TLt(typeEnv, e1, e2, tr1, tr2)
        case IfExp(condExp, thenExp, elseExp) =>
          val tr1 = condExp.typeSolve(typeEnv, MLBoolType)
          val tr2 = thenExp.typeSolve(typeEnv, myType)
          val tr3 = elseExp.typeSolve(typeEnv, myType)
          TIf(
            typeEnv,
            condExp,
            thenExp,
            elseExp,
            tr1,
            tr2,
            tr3,
            this.getType(typeEnv, myType)
          )
        case Var(n) =>
          TVar(typeEnv, Var(n), this.getType(typeEnv, myType))
        case LetExp(variable, valueExp, inExp) =>
          val tr1 =
            valueExp.typeSolve(typeEnv, valueExp.getTypeWithoutAnswer(typeEnv))
          val tr2 =
            inExp.typeSolve((variable.name, tr1.mlType) :: typeEnv, myType)
          TLet(
            typeEnv,
            variable,
            valueExp,
            inExp,
            tr1,
            tr2,
            this.getType(typeEnv, myType)
          )
        case FunExp(param, body) =>
          val a = this.getType(typeEnv, myType).asInstanceOf[MLFunType]
          val solvedBody =
            body.typeSolve((param.name, a.arg) :: typeEnv, a.body)
          TFun(typeEnv, param, body, solvedBody, a)
        case FunCall(funName, arg) =>
          val t12 =
            funName
              .getType(typeEnv, funName.getTypeWithoutAnswer(typeEnv))
              .asInstanceOf[MLFunType]
          val tr1 = funName.typeSolve(typeEnv, t12)
          val tr2 = arg.typeSolve(typeEnv, t12.arg)
          TApp(typeEnv, funName, arg, tr1, tr2, this.getType(typeEnv, myType))
        case LetRecExp(variable, RecFunExp(_, param, body), inExp) =>
          val xType = variable.getTypeWithoutAnswer(typeEnv)
          val yType = param.getTypeWithoutAnswer(typeEnv)
          val tr1 = body.typeSolve(
            (variable.name, xType) :: (param.name, yType) :: typeEnv,
            myType
          )
          val tr2 = inExp.typeSolve((variable.name, xType) :: typeEnv, myType)
          TLetRec(
            typeEnv,
            variable,
            param,
            body,
            inExp,
            tr1,
            tr2,
            this.getType(typeEnv, myType)
          )
        case EList(left, right) =>
          val t = this.getType(typeEnv, myType)
          val a = myType.asInstanceOf[MLListType]
          val tr1 = left.typeSolve(typeEnv, a.lst)
          right match {
            case EmptyList =>
              TCons(typeEnv, left, right, tr1, TNil(typeEnv, t), t)
            case _ =>
              val tr2 = right.typeSolve(typeEnv, a)
              TCons(
                typeEnv,
                left,
                right,
                tr1,
                tr2,
                this.getType(typeEnv, myType)
              )
          }
        case Match(e1: Var, patterns: List[Pattern]) =>
          patterns match {
            case Pattern(EmptyList, e2) :: Pattern(EList(Var(x), Var(y)), e3) :: List() =>
              val e1Type = e1.getTypeWithoutAnswer(typeEnv)
              val tr1 = e1.typeSolve(typeEnv, e1Type)
              val tr2 = e2.typeSolve(typeEnv, e1Type)
              val tr3 =
                e3.typeSolve((x, e1Type) :: (y, e1Type) :: typeEnv, myType)
              TMatch(
                typeEnv,
                e1,
                e2,
                Var(x),
                Var(y),
                e3,
                tr1,
                tr2,
                tr3,
                this.getType(typeEnv, myType)
              )
            case _ =>
              throw new Exception("構文エラー")
          }
      }
    }
    def solve(env: Env): Rule = {
      this match {
        case IntVal(n) =>
          EInt(env, IntVal(n))
        case BoolVal(b) =>
          EBool(env, BoolVal(b))
        case Var(n) =>
          EVar(env, Var(n))
        case InfixExp(e1, Plus, e2) =>
          val r1 = e1.solve(env)
          val r2 = e2.solve(env)
          val i3 = r1.value.asInstanceOf[IntVal] + r2.value
            .asInstanceOf[IntVal]
          EPlus(env, e1, e2, i3, r1, r2, BPlus(r1.value, r2.value, i3))
        case InfixExp(IntVal(0), Minus, e2) =>
          val r2 = e2.solve(env)
          val i3 = IntVal(
            -1 * r2.value
              .asInstanceOf[IntVal]
              .value
          )
          i3.solve(env)
        case InfixExp(e1, Minus, e2) =>
          val r1 = e1.solve(env)
          val r2 = e2.solve(env)
          val i3 = r1.value.asInstanceOf[IntVal] - r2.value
            .asInstanceOf[IntVal]
          EMinus(env, e1, e2, i3, r1, r2, BMinus(r1.value, r2.value, i3))
        case InfixExp(e1, Asterisk, e2) =>
          val r1 = e1.solve(env)
          val r2 = e2.solve(env)
          val i3 = r1.value.asInstanceOf[IntVal] * r2.value
            .asInstanceOf[IntVal]
          ETimes(env, e1, e2, i3, r1, r2, BTimes(r1.value, r2.value, i3))
        case InfixExp(e1, LessThan, e2) =>
          val r1 = e1.solve(env)
          val r2 = e2.solve(env)
          val i3 = r1.value.asInstanceOf[IntVal] < r2.value
            .asInstanceOf[IntVal]
          ELt(env, e1, e2, i3, r1, r2, BLt(r1.value, r2.value, i3))
        case IfExp(condExp, thenExp, elseExp) =>
          val r1 = condExp.solve(env)
          if (r1.value.asInstanceOf[BoolVal].value) {
            val r2 = thenExp.solve(env)
            EIfT(env, condExp, thenExp, elseExp, r1, r2)
          } else {
            val r3 = elseExp.solve(env)
            EIfF(env, condExp, thenExp, elseExp, r1, r3)
          }
        case LetExp(variable, valueExp, inExp) =>
          val r1 = valueExp.solve(env)
          val r2 = inExp.solve((variable.name, r1.value) :: env)
          ELet(env, variable, valueExp, inExp, r1, r2)
        case LetRecExp(variable, fun, inExp) =>
          val newEnv = (variable.name, fun.solve(env).value) :: env
          val in = inExp.solve(newEnv)
          ELetRec(env, variable, fun, inExp, in)
        case FunExp(variable: Var, body: Exp) =>
          EFun(env, variable, body, EClosure(env, variable, body))
        case RecFunExp(variable: Var, param: Var, body: Exp) =>
          ERecFun(
            env,
            variable,
            param,
            body,
            ERecClosure(env, variable, param, body)
          )
        case FunCall(funName: Exp, arg: Exp) =>
          funName match {
            case RecFunExp(v, p, b) =>
              val r1 = funName.solve(env)
              val recClosure = r1.value.asInstanceOf[RecClosure]
              val r2 = arg.solve(env)
              val r3 = recClosure.recFunExp.body.solve(
                (recClosure.recFunExp.param.name, r2.value) ::
                  (recClosure.recFunExp.variable.name, recClosure)
                  :: recClosure.env
              )
              EAppRec(env, funName, arg, r1, r2, r3)
            case RecClosure(ce, RecFunExp(v, p, b)) =>
              val r1 = funName.solve(ce)
              val recClosure = r1.value.asInstanceOf[RecClosure]
              val r2 = arg.solve(ce)
              val r3 = recClosure.recFunExp.body.solve(
                (recClosure.recFunExp.param.name, r2.value) ::
                  (recClosure.recFunExp.variable.name, recClosure)
                  :: recClosure.env
              )
              EAppRec(ce ::: env, funName, arg, r1, r2, r3)
            case _ =>
              val r1 = funName.solve(env)
              r1.value match {
                case RecClosure(ce, RecFunExp(v, p, b)) =>
                  val r2 = arg.solve(env)
                  val r3 =
                    b.solve((p.name, r2.value) :: (v.name, r1.value) :: ce)
                  EAppRec(ce ::: env, funName, arg, r1, r2, r3)
                case Closure(ce, FunExp(p, b)) =>
                  val r2 = arg.solve(env)
                  val r3 = b.solve((p.name, r2.value) :: ce)
                  EApp(env, funName, arg, r1, r2, r3)
              }
          }
        case EmptyList =>
          ENil(env, EmptyList)
        case EList(e1, e2) =>
          val r1 = e1.solve(env)
          val r2 = e2.solve(env)
          ECons(env, e1, e2, r1, r2)
        case Match(e0, Pattern(p, e) :: List()) =>
          val r1 = e0.solve(env)
          val v = r1.value
          val mr = p.matches(v)
          val r2 = e.solve(mr.getEnv.append(env))
          EMatchM1(env, e0, p, e, r1, mr, r2)
        case Match(e0, Pattern(p, e) :: c) =>
          val r1 = e0.solve(env)
          val v = r1.value
          if (p.checkMatching(v)) {
            val mr = p.matches(v)
            val r2 = e.solve(mr.getEnv.append(env))
            EMatchM2(env, e0, p, e, c, r1, mr, r2)
          } else {
            val nmr = p.notMatch(v)
            val r2 = Match(e0, c).solve(env)
            EMatchN(env, e0, p, e, c, r1, nmr, r2)
          }
        case _ =>
          throw new Exception("未対応")
      }
    }
    def checkMatching(v: Exp): Boolean = {
      (this, v) match {
        case (EmptyList, EmptyList) =>
          true
        case (WildCard, _) =>
          true
        case (EList(p1, p2), EList(v1, v2)) =>
          p1.checkMatching(v1) && p2.checkMatching(v2)
        case (EmptyList, EList(v1, v2)) =>
          false
        case (EList(p1, p2), EmptyList) =>
          false
        case (x, v) =>
          true
      }
    }
    def matches(v: Exp): MatchRule = {
      (this, v) match {
        case (EmptyList, EmptyList) =>
          MNil(List(), EmptyList, EmptyList)
        case (WildCard, _) =>
          MWild(List(), v)
        case (EList(p1, p2), EList(v1, v2)) =>
          val mr1 = p1.matches(v1)
          val mr2 = p2.matches(v2)
          MCons(mr2.getEnv.appendNoDouble(mr1.getEnv), p1, p2, v1, v2, mr1, mr2)
        case (x, v) =>
          val env: Env = List((x.string, v))
          MVar(env, x, v)
      }
    }
    def notMatch(v: Exp): NotMatchRule = {
      (this, v) match {
        case (EmptyList, EList(v1, v2)) =>
          NMConsNil(v1, v2)
        case (EList(p1, p2), EmptyList) =>
          NMNilCons(p1, p2)
        case (EList(p1, p2), EList(v1, v2)) =>
          if (!p1.checkMatching(v1)) {
            val nmr = p1.notMatch(v1)
            NMConsConsL(p1, p2, v1, v2, nmr)
          } else if (!p2.checkMatching(v2)) {
            val nmr = p2.notMatch(v2)
            NMConsConsR(p1, p2, v1, v2, nmr)
          } else {
            throw new Exception("not Matchが誤って呼ばれている")
          }
      }
    }
    def string: String = {
      this match {
        case IntVal(n)  => s"$n"
        case BoolVal(b) => s"$b"
        case Var(n)     => s"$n"
        case WildCard   => "_"
        case InfixExp(IntVal(0), Minus, right) =>
          s"-${right.string}"
        case InfixExp(left, op, right) =>
          s"(${left.string} ${opMap(op)} ${right.string})"
        case IfExp(condExp, thenExp, elseExp) =>
          s"if ${condExp.string} then ${thenExp.string} else ${elseExp.string}"
        case LetExp(variable, valueExp, inExp) =>
          s"let ${variable.string} = ${valueExp.string} in ${inExp.string}"
        case LetRecExp(variable, fun, inExp) =>
          s"let rec ${variable.string} = ${FunExp(fun.param, fun.body).string
            .dropRight(1)
            .drop(1)} in ${inExp.string}"
        case FunExp(param, body) =>
          val paramsStr = param.name.mkString
          s"(fun $paramsStr -> ${body.string})"
        case RecFunExp(v, param, body) =>
          val paramsStr = param.name.mkString
          s"(fun $paramsStr -> ${body.string})"
        case Closure(e, FunExp(param, body)) =>
          val funString = FunExp(param, body).string
          s"(${e.string}) [${funString.dropRight(1).drop(1)}]"
        case RecClosure(e, RecFunExp(v, param, body)) =>
          val funString = FunExp(param, body).string
          s"(${e.string}) [rec ${v.string} = ${funString.dropRight(1).drop(1)}]"
        case FunCall(funName, arg) =>
          s"(${funName.string} ${arg.string})"
        case EList(left, right) =>
          s"(${left.string} :: ${right.string})"
        case EmptyList =>
          s"[]"
        case Pattern(left, right) =>
          s"| ${left.string} -> ${right.string} "
        case Match(v, patterns) =>
          s"match ${v.string} with " + patterns
            .map(p => p.string)
            .mkString
            .drop(1)
      }
    }
  }
  case class IntVal(value: Int) extends Exp {
    def +(other: IntVal): IntVal = {
      IntVal(value + other.value)
    }
    def -(other: IntVal): IntVal = {
      IntVal(value - other.value)
    }
    def *(other: IntVal): IntVal = {
      IntVal(value * other.value)
    }
    def <(other: IntVal): BoolVal = {
      BoolVal(value < other.value)
    }
  }
  case class Var(name: String) extends Exp
  case class BoolVal(value: Boolean) extends Exp
  case class InfixExp(leftExp: Exp, op: Op, rightExp: Exp) extends Exp
  case class IfExp(condExp: Exp, thenExp: Exp, elseExp: Exp) extends Exp
  case class LetExp(variable: Var, valueExp: Exp, inExp: Exp) extends Exp
  case class FunExp(param: Var, body: Exp) extends Exp
  case class FunCall(funName: Exp, arg: Exp) extends Exp
  case class Closure(env: List[(String, Exp)], funExp: FunExp) extends Exp
  case class RecFunExp(variable: Var, param: Var, body: Exp) extends Exp
  case class RecClosure(env: List[(String, Exp)], recFunExp: RecFunExp)
      extends Exp
  case class LetRecExp(variable: Var, valueExp: RecFunExp, inExp: Exp)
      extends Exp
  sealed class ListExp extends Exp
  case class EList(first: Exp, second: Exp) extends ListExp
  object EmptyList extends ListExp
  case class Pattern(left: ListExp, right: Exp) extends Exp
  case class Match(v: Var, patterns: List[Pattern]) extends Exp
  object WildCard extends Exp
}
