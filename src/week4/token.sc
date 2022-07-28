// This produces more meaningful tokens and
// also filters out comments and whitespaces
//
// call with
//
//  amm token.sc
//

// load the lexer
import $file.lexer
import lexer._ 


// The tokens for the WHILE language

abstract class Token 
case object T_SEMI extends Token
case object T_LPAREN extends Token
case object T_RPAREN extends Token
case class T_ID(s: String) extends Token
case class T_OP(s: String) extends Token
case class T_NUM(n: Int) extends Token
case class T_KWD(s: String) extends Token
case class T_STR(s: String) extends Token


val token : PartialFunction[(String, String), Token] = {
  case ("s", _) => T_SEMI
  case ("p", "{") => T_LPAREN
  case ("p", "}") => T_RPAREN
  case ("i", s) => T_ID(s)
  case ("o", s) => T_OP(s)
  case ("n", s) => T_NUM(s.toInt)
  case ("k", s) => T_KWD(s)
  case ("str", s) => T_STR(s)

}

// by using collect we filter out all unwanted tokens
def tokenise(s: String) : List[Token] = 
  lexing_simp(WHILE_REGS, s).collect(token)




@arg(doc = "Tokens for fib and loops programs.")
@main
def main() = {
  println("Fib program")
  println(tokenise(prog2))
  println("Loops program")
  println(tokenise(prog3))

  for (i <- 0 to 20 by 5) {
    println(f"$i%2.0f: ${time(tokenise(prog3 * i))._2}")
  }

}




// Primes program
//================

/*
end := 100;
n := 2;
while (n < end) do {
  f := 2;
  tmp := 0;
  while ((f < n / 2 + 1) && (tmp == 0)) do {
    if ((n / f) * f == n) then  { tmp := 1 } else { skip };
    f := f + 1
  };
  if (tmp == 0) then { write(n) } else { skip };
  n  := n + 1
}
*/

// Factors program
//=================

/*
write "Input n please";
read n;
write "The factors of n are";
f := 2;
while n != 1 do {
    while (n / f) * f == n do {
        write f;
        n := n / f
    };
    f := f + 1
}
*/


// runs with amm2 and amm3
