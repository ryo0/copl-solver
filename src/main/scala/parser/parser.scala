package parser

import tokenizer.token._
import ast._

object parser {
  val opMap = Map(PlusToken -> Plus, MinusToken -> Minus)
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
      val (pr1, rest) = parsePrimary(tokens)
      rest match {
        case AsteriskToken :: rest2 =>
          val (pr2, rest3) = parsePrimary(rest2)
          val infix = InfixExp(pr1, Asterisk, pr2)
        rest3 match {
          case AsteriskToken :: rest4 =>
          val (mul, rest5) = parseMul(rest4)
          (InfixExp(infix, Asterisk, mul), rest5)
          case _ =>
            (infix, rest3)
        }
        case _ =>
          (pr1, rest)
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
