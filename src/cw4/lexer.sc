// call with
//
//  amm lexer.sc
//
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp
// records for extracting strings or tokens
case class RECD(x: String, r: Rexp) extends Rexp
// r{n}
case class NTIMES(r: Rexp, n: Int) extends Rexp
//[c1, c2, . . . , cn]
case class RANGE(cs:List[Char]) extends Rexp
// r+
case class PLUS(r: Rexp) extends Rexp
//r ?
case class OPTIONAL(r:Rexp) extends Rexp
// ...m
case class UPTO(r:Rexp, m : Int) extends Rexp
// n...
case class FROM(r:Rexp, n : Int) extends Rexp
// n..m
case class BETWEEN(r:Rexp, n : Int, m :Int) extends Rexp
//∼ r
case class NOT(r:Rexp) extends Rexp

case class CFUN(f: Char => Boolean) extends Rexp

// values
abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(v1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Stars(vs: List[Val]) extends Val
case class Rec(x: String, v: Val) extends Val


// some convenience for typing in regular expressions

def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
implicit def string2rexp(s : String) : Rexp =
  charlist2rexp(s.toList)


implicit def RexpOps(r: Rexp) = new {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

implicit def stringOps(s: String) = new {
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
  def $ (r: Rexp) = RECD(s, r)
}


// the nullable function: tests whether the regular
// expression can recognise the empty string
def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case RECD(_, r1) => nullable(r1)
  //r{n}
  case NTIMES(r, i) => if (i == 0) true else nullable(r)
  //[c1, c2, . . . , cn]
  case RANGE(cs:List[Char]) => if(cs.isEmpty) true else false
  // r+
  case PLUS(r) => nullable(r)
  //r ?
  case OPTIONAL(r) => true
  // ...m
  case UPTO(r, m) => true
  // n ...
  case FROM(r, n) => if (n == 0) true else nullable(r)
  // {n..m}
  case BETWEEN(r,n,m) => if(n ==0 || m ==0) true else nullable(r)
  //∼ r
  case NOT(r) => !nullable(r)

  case CFUN(_) => false
}

// the derivative of a regular expression w.r.t. a character
def der(c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) =>
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r1) => SEQ(der(c, r1), STAR(r1))
  case RECD(_, r1) => der(c, r1)
  //r{n}
  case NTIMES(r, i) =>
    if (i == 0) ZERO else SEQ(der(c, r), NTIMES(r, i - 1))
  //[c1, c2, . . . , cn]
  case RANGE(cs:List[Char]) =>
    if (cs.contains(c)) ONE
    else ZERO
  // r+
  case PLUS(r) => SEQ(der(c, r), STAR(r))
  // r?
  case OPTIONAL(r) => der(c, r)
  // ...m
  case UPTO(r,m) =>
    if (m == 0) ZERO else SEQ(der(c, r), UPTO(r, m - 1))
  // n ...
  case FROM(r, n) =>
    if(n == 0) SEQ(der(c, r), STAR(r))
    else SEQ(der(c, r),FROM(r,n-1))
  // {n..m}
  case  BETWEEN(r,n,m) =>
    if ( m ==0 ) ZERO
    else if(n ==0 ){
      if (m == 0) ZERO else SEQ(der(c, r),  BETWEEN(r,0, m - 1))
    }
    else{
      SEQ(der(c, r), BETWEEN(r,n-1,m-1))
    }
  //∼ r
  case NOT(r:Rexp) => NOT(der(c, r))

  case CFUN(f) => if(f(c)) ONE else ZERO
}

// extracts a string from a value
def flatten(v: Val) : String = v match {
  case Empty => ""
  case Chr(c) => c.toString
  case Left(v) => flatten(v)
  case Right(v) => flatten(v)
  case Sequ(v1, v2) => flatten(v1) ++ flatten(v2)
  case Stars(vs) => vs.map(flatten).mkString
  case Rec(_, v) => flatten(v)

}

// extracts an environment from a value;
// used for tokenising a string
def env(v: Val) : List[(String, String)] = v match {
  case Empty => Nil
  case Chr(c) => Nil
  case Left(v) => env(v)
  case Right(v) => env(v)
  case Sequ(v1, v2) => env(v1) ::: env(v2)
  case Stars(vs) => vs.flatMap(env)
  case Rec(x, v) => (x, flatten(v))::env(v)

}

// The injection and mkeps part of the lexer
//===========================================

def mkeps(r: Rexp) : Val = r match {
  case ONE => Empty
  case ALT(r1, r2) =>
    if (nullable(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
  case SEQ(r1, r2) => Sequ(mkeps(r1), mkeps(r2))
  case STAR(r) => Stars(Nil)
  case RECD(x, r) => Rec(x, mkeps(r))

  case RANGE(cs:List[Char]) => Empty
  case PLUS(r) =>  Stars(List(mkeps(r)))
  case OPTIONAL(r) => Stars(Nil)
  case NTIMES(r,n:Int) => Stars(List.fill(n)(mkeps(r)))
}

def inj(r: Rexp, c: Char, v: Val) : Val = (r, v) match {
  case (STAR(r), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
  case (SEQ(r1, r2), Sequ(v1, v2)) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Left(Sequ(v1, v2))) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Right(v2)) => Sequ(mkeps(r1), inj(r2, c, v2))
  case (ALT(r1, r2), Left(v1)) => Left(inj(r1, c, v1))
  case (ALT(r1, r2), Right(v2)) => Right(inj(r2, c, v2))
  case (CHAR(d), Empty) => Chr(c)
  case (RECD(x, r1), _) => Rec(x, inj(r1, c, v))


  case(RANGE(_),Empty) => Chr(c)
  case(PLUS(r),Sequ(v1,Stars(vs))) => Stars(inj(r,c,v1)::vs)
  case(OPTIONAL(r),_) => Stars(inj(r,c,v)::Nil)
  case(NTIMES(r,n),Sequ(v1,Stars(vs))) => Stars(inj(r,c,v1)::vs)
}

// some "rectification" functions for simplification
def F_ID(v: Val): Val = v
def F_RIGHT(f: Val => Val) = (v:Val) => Right(f(v))
def F_LEFT(f: Val => Val) = (v:Val) => Left(f(v))
def F_ALT(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
  case Right(v) => Right(f2(v))
  case Left(v) => Left(f1(v))
}
def F_SEQ(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
  case Sequ(v1, v2) => Sequ(f1(v1), f2(v2))
}
def F_SEQ_Empty1(f1: Val => Val, f2: Val => Val) =
  (v:Val) => Sequ(f1(Empty), f2(v))
def F_SEQ_Empty2(f1: Val => Val, f2: Val => Val) =
  (v:Val) => Sequ(f1(v), f2(Empty))

def F_ERROR(v: Val): Val = throw new Exception("error")

// simplification
def simp_2(r: Rexp): (Rexp, Val => Val) = r match {
  case ALT(r1, r2) => {
    val (r1s, f1s) = simp_2(r1)
    val (r2s, f2s) = simp_2(r2)
    (r1s, r2s) match {
      case (ZERO, _) => (r2s, F_RIGHT(f2s))
      case (_, ZERO) => (r1s, F_LEFT(f1s))
      case _ => if (r1s == r2s) (r1s, F_LEFT(f1s))
      else (ALT (r1s, r2s), F_ALT(f1s, f2s))
    }
  }
  case SEQ(r1, r2) => {
    val (r1s, f1s) = simp_2(r1)
    val (r2s, f2s) = simp_2(r2)
    (r1s, r2s) match {
      case (ZERO, _) => (ZERO, F_ERROR)
      case (_, ZERO) => (ZERO, F_ERROR)
      case (ONE, _) => (r2s, F_SEQ_Empty1(f1s, f2s))
      case (_, ONE) => (r1s, F_SEQ_Empty2(f1s, f2s))
      case _ => (SEQ(r1s,r2s), F_SEQ(f1s, f2s))
    }
  }
  case r => (r, F_ID)
}

// lexing functions including simplification
def lex_simp(r: Rexp, s: List[Char]) : Val = s match {
  case Nil => if (nullable(r)) mkeps(r) else
  { throw new Exception("lexing error") }
  case c::cs => {
    val (r_simp, f_simp) = simp_2(der(c, r))
    inj(r, c, f_simp(lex_simp(r_simp, cs)))
  }
}

def lexing_simp(r: Rexp, s: String) =
  env(lex_simp(r, s.toList))

def simp(r: Rexp) : Rexp = r match {
  case ALT(r1, r2) => (simp(r1), simp(r2)) match {
    case (ZERO, r2s) => r2s
    case (r1s, ZERO) => r1s
    case (r1s, r2s) => if (r1s == r2s) r1s else ALT (r1s, r2s)
  }
  case SEQ(r1, r2) =>  (simp(r1), simp(r2)) match {
    case (ZERO, _) => ZERO
    case (_, ZERO) => ZERO
    case (ONE, r2s) => r2s
    case (r1s, ONE) => r1s
    case (r1s, r2s) => SEQ(r1s, r2s)
  }
  case r => r
}


// the derivative w.r.t. a string (iterates der)
def ders(s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, simp(der(c, r)))
}

// the main matcher function
def matcher(r: Rexp, s: String) : Boolean =
  nullable(ders(s.toList, r))


val LETTER: Rexp = RANGE("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".toList)
val SYM : Rexp= LETTER | RANGE("._<>=;,\\:".toList)
val DIGIT: Rexp = RANGE("0123456789".toList)
val KEYWORD : Rexp = "while" | "if" | "then" | "else" | "do" | "for" | "to" | "true" |
  "false" | "read" | "write" | "skip" | "upto"
val SEMI: Rexp = ";"
val OP: Rexp = "+" | "-" | "*" | "%" | "/" | "==" |
  "!=" | ">" | "<" | "<=" | ">=" | ":=" | "&&" | "||"
val WHITESPACE: Rexp = PLUS(" " | "\n" | "\t" | "\r")
val STRING: Rexp = "\"" ~ (SYM | WHITESPACE | DIGIT).% ~ "\""
val PAREN: Rexp = "{" | "}" | "(" | ")"
val IDENT: Rexp =  PLUS(LETTER) ~ PLUS("_" |LETTER |DIGIT)
val NUMBER : Rexp = DIGIT | RANGE("123456789".toList) ~ DIGIT.%
val COMMENT :Rexp = "//" ~ (SYM | " " | DIGIT).% ~ ("\n\r" | "\n" | "\r\n")
//val TYPE :Rexp = "Void" | "Int" | "Double"


val WHILE_REGS :Rexp = (("k" $ KEYWORD) |
  ("o" $ OP) |
  ("i" $ IDENT) |
  ("l" $ LETTER)|
  ("s" $ SEMI) |
  ("sym" $ SYM) |
  ("str" $ STRING) |
  ("p" $ PAREN) |
  ("w" $ WHITESPACE)|
  ("n" $ NUMBER)|
  ("c" $ COMMENT)).%



@arg(doc = "Test 1")
@main
def main() = {
  //Rang
  val reg1 : Rexp = RANGE("abcd".toList)
  val str1: String = "c"
  println(lex_simp(reg1,str1.toList))
  println(lex_simp(RANGE(Nil),"".toList))
  println("============")
  //Pls
  val reg2 : Rexp = PLUS("asd"|"sd")
  val str2: String = "sdasd"
  println(lex_simp(reg2,str2.toList))
  println(lex_simp(PLUS(""|"sd"),"".toList))
  println("============")
  //Opti
  val reg3 : Rexp = OPTIONAL(""|"sd")
  val str3: String = "sd"
  println(lex_simp(reg3,str3.toList))
  println(lex_simp(reg3,"".toList))
  println(lex_simp(OPTIONAL("sd"),"".toList))
  println(lex_simp(OPTIONAL(""),"".toList))
  println("============")
  //Ntims
  val reg4 : Rexp = NTIMES("a",3)
  val reg5 : Rexp = NTIMES("a" | ONE,3)
  println(lex_simp(reg4,"aaa".toList))
  println(lex_simp(reg5,"aa".toList))
  println("============")

  val prog1 = "read n;"
  println(s"test: $prog1")
  println(lexing_simp(WHILE_REGS, prog1))
}

@arg(doc = "Test 1")
@main
def test1(): Unit ={
  val r1 = "abc"
  println("start")
  println(mkeps(r1))
  println("finish")

}


