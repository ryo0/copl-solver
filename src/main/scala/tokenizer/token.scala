package tokenizer

object token {
  sealed class Token
  case class IntToken(value: Int) extends Token

  case class VarToken(value: String) extends Token

  object LParen extends Token

  object RParen extends Token

  object PlusToken extends Token

  object MinusToken extends Token

  object AsteriskToken extends Token

  object SlashToken extends Token

  object LessThanToken extends Token

  object GreaterThanToken extends Token

  object EqualToken extends Token

  object IfToken extends Token

  object ThenToken extends Token

  object ElseToken extends Token

  object LetToken extends Token

  object InToken extends Token

  object ArrowToken extends Token

  object FunToken extends Token

  object TrueToken extends Token

  object FalseToken extends Token

  object RecToken extends Token

  object MatchToken extends Token

  object EmptyListToken extends Token

  object ConsToken extends Token

  object OrToken extends Token
}
