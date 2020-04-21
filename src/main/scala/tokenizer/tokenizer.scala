package tokenizer

import token._

object tokenizer {
  val tokenMap: Map[Char, Token] =
    Map(
      '+' -> PlusToken,
      '-' -> MinusToken,
      '*' -> AsteriskToken,
      '/' -> SlashToken,
      '(' -> LParen,
      ')' -> RParen,
      '<' -> LessThanToken,
      '>' -> GreaterThanToken,
      '=' -> EqualToken
    )
  val reservedWordMap: Map[String, Token] = Map(
    "if" -> IfToken,
    "then" -> ThenToken,
    "else" -> ElseToken,
    "in" -> InToken,
    "let" -> LetToken,
    "fun" -> FunToken
  )

  def tokenize(str: String): List[Token] = {
    def tokenizeSub(str: String, acm: List[Token]): List[Token] = {
      if (str.isBlank) {
        return acm
      }
      val c = str(0)
      val rest = str.slice(1, str.length)
      if (rest.length > 0 && c == '-' && rest.head == '>') {
        return tokenizeSub(rest.tail, acm :+ ArrowToken)
      }
      if (tokenMap.keySet.contains(c)) {
        return tokenizeSub(rest, acm :+ tokenMap(c))
      }
      c match {
        case ' ' =>
          tokenizeSub(rest, acm)
        case '\n' =>
          tokenizeSub(rest, acm)
        case _ =>
          if (c.isDigit) {
            val (rest, intToken) = tokenizeInt(str)
            tokenizeSub(rest, acm :+ intToken)
          } else if (c.isLetter) {
            val (rest, letterToken) = tokenizeLetter(str)
            tokenizeSub(rest, acm :+ letterToken)
          } else {
            throw new Exception("tokenizerに不正な文字が渡されました。" + c)
          }
      }
    }
    tokenizeSub(str, List())
  }

  def retVarOrToken(str: String): Token = {
    if (reservedWordMap.keySet.contains(str)) {
      return reservedWordMap(str)
    }
    VarToken(str)
  }

  def tokenizeLetter(str: String): (String, Token) = {
    def tokenizeLetterSub(str: String, acm: String): (String, Token) = {
      if (str.isBlank) {
        return (str, retVarOrToken(acm))
      }
      val c = str(0)
      val rest = str.slice(1, str.length)
      if (c.isLetter) {
        tokenizeLetterSub(rest, acm :+ c)
      } else {
        (str, retVarOrToken(acm))
      }
    }
    tokenizeLetterSub(str, "")
  }

  def tokenizeInt(str: String): (String, IntToken) = {
    def tokenizeIntSub(str: String, acm: String): (String, IntToken) = {
      if (str.isBlank) {
        return (str, IntToken(acm.toInt))
      }
      val c = str(0)
      val rest = str.slice(1, str.length)
      if (c.isDigit) {
        tokenizeIntSub(rest, acm :+ c)
      } else {
        (str, IntToken(acm.toInt))
      }
    }
    tokenizeIntSub(str, "")
  }
}
