package parser

import tokenizer.token._
import ast._

object parser {
  val opMap: Map[Token, Op] = Map(PlusToken -> Plus, MinusToken -> Minus, AsteriskToken -> Asterisk, SlashToken -> Slash,
    LessThanToken -> LessThan, GreaterThanToken -> GreaterThan)

  def parseExp(tokens: List[Token]): (Exp, List[Token]) = {
    tokens match {
      case IfToken :: rest =>
        parseIf(tokens)
      case LetToken :: rest =>
        parseLet(tokens)
      case _ =>
        parseRelational(tokens)
    }
  }

  def parseLet(tokens: List[Token]) : (LetExp, List[Token]) = {
    tokens match {
      case LetToken  :: VarToken(n) :: EqualToken :: rest =>
        val (value, rest2) = parseExp(rest)
        rest2 match {
          case InToken :: rest3 =>
            val (in, rest4) = parseExp(rest3)
            (LetExp(Var(n), value, in), rest4)
          case _ =>
            throw new Exception("letにinがない")
        }
      case _ =>
        throw new Exception("letが let x = 2...という形式でない")
    }
  }

  def parseIf(tokens: List[Token]): (IfExp, List[Token]) = {
    tokens match {
      case IfToken :: rest =>
        val (condExp, rest2) = parseExp(rest)
        rest2 match {
          case ThenToken :: rest3 =>
            val (thenExp, rest4) = parseExp(rest3)
            rest4 match {
            case ElseToken :: rest5 =>
                val (elseExp, rest6) = parseExp(rest5)
                (IfExp(condExp, thenExp, elseExp), rest6)
            case _ =>
                throw new Exception("ifにelse節がない")
            }
          case _ =>
            throw new Exception("ifにthen節がない")
        }
      case _ => {
        throw new Exception("parseIf if以外が渡された")
      }
    }
  }

  def parseRelational(tokens: List[Token]): (Exp, List[Token]) = {
    val (sum1, rest) = parseSum(tokens)
    rest match {
      case (LessThanToken | GreaterThanToken) :: rest2 =>
        val (sum2, rest3) = parseSum(rest2)
        val infixExp = InfixExp(sum1, opMap(rest.head), sum2)
        rest3 match {
          case (LessThanToken | GreaterThanToken)  :: rest4 =>
            val (sum, rest5) = parseRelational(rest4)
            (InfixExp(infixExp, opMap(rest3.head), sum), rest5)
          case _ =>
            (infixExp, rest3)
        }
      case _ =>
        (sum1, rest)
    }
  }

  def parseSum(tokens: List[Token]): (Exp, List[Token]) = {
    val (mul1, rest) = parseMul(tokens)
    rest match {
      case (PlusToken | MinusToken) :: rest2 =>
        val (mul2, rest3) = parseMul(rest2)
        val infix = InfixExp(mul1, opMap(rest.head), mul2)
        rest3 match {
          case (PlusToken | MinusToken)  :: rest4 =>
            val (mul, rest5) = parseSum(rest4)
            (InfixExp(infix, opMap(rest3.head), mul), rest5)
          case _ =>
            (infix, rest3)
        }
      case _ =>
        (mul1, rest)
    }
  }
  def parseMul(tokens: List[Token]): (Exp, List[Token])= {
      val (pr1, rest) = parseUnary(tokens)
      rest match {
        case (AsteriskToken | SlashToken) :: rest2 =>
          val (pr2, rest3) = parseUnary(rest2)
          val infix = InfixExp(pr1, opMap(rest.head), pr2)
        rest3 match {
          case (AsteriskToken | SlashToken) :: rest4 =>
          val (mul, rest5) = parseMul(rest4)
          (InfixExp(infix, opMap(rest3.head), mul), rest5)
          case _ =>
            (infix, rest3)
        }
        case _ =>
          (pr1, rest)
      }
  }

  def parseUnary(tokens: List[Token]): (Exp, List[Token]) = {
    tokens match {
      case MinusToken :: rest =>
        val (prim, _rest) = parsePrimary(rest)
        (InfixExp(IntVal(0), Minus, prim), _rest)
      case _ =>
        parsePrimary(tokens)
    }
  }

  def parsePrimary(tokens: List[Token]): (Exp, List[Token]) = {
    tokens match {
      case LParen :: rest =>
        val (exp, _rest) = parseExp(rest)
        _rest match {
          case RParen :: __rest =>
            (exp, __rest)
          case _ =>
            println(_rest)
            throw new Exception("erorr, カッコが閉じてない")
        }
      case IntToken(n) :: rest =>
        (IntVal(n), rest)
      case VarToken(n) :: rest =>
        (Var(n), rest)
      case _ =>
        parseExp(tokens)
    }
  }
}
