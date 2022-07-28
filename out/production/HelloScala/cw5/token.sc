// call with
//
//  amm token.sc test1
//

// load the lexer
// The tokens for the FUN language
import $file.lexer
import lexer._

abstract class Token
case object T_SEMI extends Token
case object T_COM extends Token
case object T_COMMA extends Token
case object T_COLON  extends Token
case object T_QUOTA  extends Token
case class  T_PAREN(s: String) extends Token
case class T_ID(s: String) extends Token
case class T_OP(s: String) extends Token
case class T_NUM(n: Int) extends Token
case class T_FLOAT(f: Double) extends Token
case class T_KWD(s: String) extends Token




val token : PartialFunction[(String, String), Token] = {
  case ("k", s) => T_KWD(s)
  case ("i", s) => T_ID(s)
  case ("o", s) => T_OP(s)

  case ("n", s) => T_NUM(s.toInt)
  case ("f", s) => T_FLOAT(s.toDouble)

  case ("p", s) => T_PAREN(s)
  case ("s", _) => T_SEMI
  case ("c",_) => T_COMMA
  case ("co",_) => T_COLON
  case ("q",_) => T_QUOTA

//  case("sym",s) => T_SYM(s)
//  case ("str", s) => T_STR(s)
//  case ("l",s) => T_LETTER(s)
//  case ("c", _) => T_COM
}

// by using collect we filter out all unwanted tokens
def tokenise(s: String) : List[Token] = {
  val tks = lexing_simp(FUN_REGS, s).collect(token)
  if (tks.length != 0) tks
  else { println (s"Tokenise Error") ; sys.exit(-1) }
}








