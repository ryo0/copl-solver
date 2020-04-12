package tokenizer

object token {
  sealed class Token
  case class IntToken(value: Int) extends Token

  object LParen extends Token

  object RParen extends Token

  object PlusToken extends Token

  object MinusToken extends Token

  object AsteriskToken extends Token
}
