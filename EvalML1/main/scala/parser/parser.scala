package parser

import tokenizer.token._
import ast._

object parser {
  val opMap: Map[Token, Op] = Map(
    PlusToken -> Plus,
    MinusToken -> Minus,
    AsteriskToken -> Asterisk,
    SlashToken -> Slash,
    LessThanToken -> LessThan,
    GreaterThanToken -> GreaterThan
  )

  def parseExp(tokens: List[Token]): (Exp, List[Token]) = {
    tokens match {
      case IfToken :: rest =>
        parseIf(tokens)
      case LetToken :: rest =>
        parseLet(tokens)
      case FunToken :: rest =>
        parseFun(tokens)
      case _ =>
        parseRelational(tokens)
    }
  }

  def parseFunCall(fun: Exp, tokens: List[Token]): (FunCall, List[Token]) = {
    if (tokens.isEmpty) {
      return (fun.asInstanceOf[FunCall], tokens)
    }
    tokens.head match {
      case LParen | IntToken(_) | VarToken(_) | FunToken =>
        val (arg, rest) = parseArg(tokens)
        arg match {
          case Some(argExp) =>
            parseFunCall(FunCall(fun, argExp), rest)
          case _ =>
            (fun.asInstanceOf[FunCall], tokens)
        }
      case _ =>
        (fun.asInstanceOf[FunCall], tokens)
    }
  }

  def parseArg(tokens: List[Token]): (Option[Exp], List[Token]) = {
    if (tokens.isEmpty) {
      return (None, tokens)
    }

    tokens.head match {
      case LParen =>
        val (exp, rest) = parseExp(tokens)
        (Some(exp), rest)
      case VarToken(n) =>
        (Some(Var(n)), tokens.tail)
      case IntToken(n) =>
        (Some(IntVal(n)), tokens.tail)
      case _ =>
        (None, tokens)
    }
  }

  def parseFun(tokens: List[Token]): (FunExp, List[Token]) = {
    tokens match {
      case FunToken :: rest =>
        val (params, rest2) = parseParams(rest)
        val (body, rest3) = parseExp(rest2)
        (FunExp(params.head, body), rest3)
      case _ =>
        throw new Exception("error: funがない")
    }
  }

  def parseParams(tokens: List[Token]): (List[Var], List[Token]) = {
    def parseParamsSub(tokens: List[Token],
                       acm: List[Var]): (List[Var], List[Token]) = {
      tokens match {
        case ArrowToken :: rest =>
          (acm, rest)
        case VarToken(n) :: rest =>
          parseParamsSub(rest, acm :+ Var(n))
        case _ =>
          throw new Exception("error: parameterにvarでないものがある")
      }
    }
    parseParamsSub(tokens, List())
  }

  def parseLet(tokens: List[Token]): (LetExp, List[Token]) = {
    tokens match {
      case LetToken :: VarToken(n) :: EqualToken :: rest =>
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
          case (LessThanToken | GreaterThanToken) :: rest4 =>
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
          case (PlusToken | MinusToken) :: rest4 =>
            val (mul, rest5) = parseSum(rest4)
            (InfixExp(infix, opMap(rest3.head), mul), rest5)
          case _ =>
            (infix, rest3)
        }
      case _ =>
        (mul1, rest)
    }
  }
  def parseMul(tokens: List[Token]): (Exp, List[Token]) = {
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
        val (exp, rest2) = parseExp(rest)
        rest2 match {
          case RParen :: rest3 =>
            val (arg, _) = parseArg(rest3)
            arg match {
              case Some(_) =>
                parseFunCall(exp, rest3)
              case _ =>
                (exp, rest3)
            }
          case _ =>
            throw new Exception("erorr, カッコが閉じてない")
        }
      case IntToken(n) :: rest =>
        (IntVal(n), rest)
      case VarToken(n) :: rest =>
        val (arg, _) = parseArg(rest)
        arg match {
          case Some(_) =>
            parseFunCall(Var(n), rest)
          case _ =>
            (Var(n), rest)
        }
      case FunToken :: _ =>
        val (fun, rest2) = parseFun(tokens)
        parseFunCall(fun, rest2)
      case _ =>
        parseExp(tokens)
    }
  }
}
