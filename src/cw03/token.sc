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
case object T_COM extends Token
case class T_PAREN(s: String) extends Token
case class T_ID(s: String) extends Token
case class T_OP(s: String) extends Token
case class T_NUM(n: Int) extends Token
case class T_KWD(s: String) extends Token
case class T_STR(s: String) extends Token
case class T_LETTER(s: String) extends Token
case class T_SYM(s: String) extends Token


val token : PartialFunction[(String, String), Token] = {
  case ("k", s) => T_KWD(s)
  case ("o", s) => T_OP(s)
  case ("l",s) => T_LETTER(s)
  case("sym",s) => T_SYM(s)
  case ("str", s) => T_STR(s)
  case ("p",s) => T_PAREN(s)
  case ("s", _) => T_SEMI
  case ("i", s) => T_ID(s)
  case ("n", s) => T_NUM(s.toInt)
//  case ("c", _) => T_COM
}

// by using collect we filter out all unwanted tokens
def tokenise(s: String) : List[Token] =
  lexing_simp(WHILE_REGS, s).collect(token)



val prog1 = """
write "Fib";
read n;
minus1 := 0;
minus2 := 1;
while n > 0 do {
       temp := minus2;
       minus2 := minus1 + minus2;
       minus1 := temp;
       n := n - 1
};
write "Result";
write minus2
"""
val prog2 = """
start := 1000;
x := start;
y := start;
z := start;
while 0 < x do {
 while 0 < y do {
  while 0 < z do { z := z - 1 };
  z := start;
  y := y - 1
 };
 y := start;
 x := x - 1
}
"""
val prog3 = """
// Find all factors of a given input number
write "Input n please";
read n;
write "The factors of n are";
f := 2;
while (f < n / 2 + 1) do {
  if ((n / f) * f == n) then { write(f) } else { skip };
  f := f + 1
}
"""
val prog4 = """
// Collatz series
//
// needs writing of strings and numbers; comments
bnd := 1;
while bnd < 101 do {
  write bnd;
  write ": ";
  n := bnd;
  cnt := 0;
  while n > 1 do {
    write n;
    write ",";

    if n % 2 == 0
    then n := n / 2
    else n := 3 * n+1;

    cnt := cnt + 1
  };

  write " => ";
  write cnt;
  write "\n";
  bnd := bnd + 1
}
"""

@arg(doc = "Tokens for four programs.")
@main
def main() = {
  println("First_ Fib program")
  println(tokenise(prog1))
  println("==========")
  println("Second_ Loops program")
  println(tokenise(prog2))
  println("==========")
  println("Third_ Calculates factors program")
  println(tokenise(prog3))
  println("==========")
  println("Forth_ Collatz program")
  println(tokenise("if x4x < 33 then 1 else 3"))
  println("==========")
}

