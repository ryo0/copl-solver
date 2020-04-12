package parser

import tokenizer.token._
import ast._

object parser {
  val opMap = Map(PlusToken -> Plus, MinusToken -> Minus, AsteriskToken -> Asterisk, SlashToken -> Slash)
  def parseExp(tokens: List[Token]): (Exp, List[Token]) = {
    val (mul1, rest) = parseMul(tokens)
    rest match {
      case (PlusToken | MinusToken) :: rest2 =>
        val (mul2, rest3) = parseMul(rest2)
        val infix = InfixExp(mul1, opMap(rest.head), mul2)
        rest3 match {
          case (PlusToken | MinusToken)  :: rest4 =>
            val (mul, rest5) = parseExp(rest4)
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
      case _ =>
        println("other", tokens)
        throw new Exception("other")
    }
  }
}
