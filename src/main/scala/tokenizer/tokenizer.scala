package tokenizer

import token._

object tokenizer {
  def tokenize(str: String): List[Token]  = {
    def tokenizeSub(str: String, acm: List[Token]): List[Token] = {
      if (str.isBlank) {
        return acm
      }
      val c = str(0)
      val rest = str.slice(1, str.length)
      c match {
        case '+' =>
          tokenizeSub(rest, acm :+ PlusToken)
        case '-' =>
          tokenizeSub(rest, acm :+ MinusToken)
        case '*' =>
          tokenizeSub(rest, acm :+ AsteriskToken)
        case '/' =>
          tokenizeSub(rest, acm :+ SlashToken)
        case ' ' =>
          tokenizeSub(rest, acm)
        case '(' =>
          tokenizeSub(rest, acm :+ LParen)
        case ')' =>
          tokenizeSub(rest, acm :+ RParen)
        case _ =>
          if (c.isDigit) {
           val (rest, intToken) = tokenizeInt(str)
            tokenizeSub(rest, acm :+ intToken)
          } else {
            throw  new Exception("tokenizerに不正な文字が渡されました。" + c)
          }
       }
    }
    tokenizeSub(str, List())
  }

  def tokenizeInt(str: String): (String, IntToken) ={
    def tokenizeIntSub(str: String, acm: String) :(String, IntToken) = {
      if (str.isBlank) {
        return (str, IntToken(acm.toInt))
      }
      val c = str(0)
      val rest = str.slice(1, str.length)
      if (c.isDigit) {
        tokenizeIntSub(rest, acm:+c)
      } else {
        (str, IntToken(acm.toInt))
      }
    }
    tokenizeIntSub(str, "")
  }
}
