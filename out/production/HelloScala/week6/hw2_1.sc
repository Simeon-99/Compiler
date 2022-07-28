//import com.sun.javafx.fxml.expression.Expression.Parser.Token
// A simple lexer inspired by work of Sulzmann & Lu
//==================================================
//
// Call the test cases with
//
//   amm lexer.sc small
//   amm lexer.sc fib
//   amm lexer.sc loops
//   amm lexer.sc email
//
//   amm lexer.sc all


// regular expressions including records
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp
case class ONEORMORE(r:Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp
case class MULTI(s: List[Char]) extends Rexp
case class ONEORO(r:Rexp) extends Rexp
case class SMALLTIMES(r:Rexp, n: Int) extends Rexp
case class LARGETIMES(r:Rexp, m: Int) extends Rexp
case class MIDDLETIMES(r:Rexp, m: Int, n: Int) extends Rexp
case class NOT(r:Rexp) extends Rexp
case class RECD(x: String, r: Rexp) extends Rexp
case class CFUN(f: Char=> Boolean) extends Rexp

def oneChar(c: Char) = (d:Char) => c==d
def all() = (c:Char)=> true
def charSet(c:List[Char]) = (d:Char) => c.contains(d)
// records for extracting strings or tokens

// values
abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(v1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Stars(vs: List[Val]) extends Val
case class Rec(x: String, v: Val) extends Val
case class plus(vs: List[Val]) extends Val
case class charSet(c:Char) extends Val
case class quMark(v:Val) extends Val
case class ntime(vs:List[Val]) extends Val
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

def nullable(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, i) => if (i == 0) true else nullable(r)
  case MULTI(s:List[Char]) =>
    if(s.isEmpty) true
    else false
  case ONEORMORE(r) => nullable(r)
  case ONEORO(_) => true
  case SMALLTIMES(r,i) => if(i>=0) true else nullable(r)
  case LARGETIMES(r,i) => if(i<=0) true else nullable(r)
  case MIDDLETIMES(r,j,i) => if(i>=0&&j<=0) true else nullable(r)
  case NOT(r) => !nullable(r)
  case RECD(_, r1) => nullable(r1)
  case CFUN(_) => false

}

def der(c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) =>
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r) => SEQ(der(c, r), STAR(r))
  case NTIMES(r1, i) =>
    if (i == 0) ZERO else SEQ(der(c, r1), NTIMES(r1, i - 1))
  case MULTI(s:List[Char]) =>
    if(s.isEmpty) ZERO
    else ALT(der(c,CHAR(s.head)),der(c,MULTI(s.tail)))
  case ONEORMORE(d) =>
    SEQ(der(c,d),STAR(d))
  case ONEORO(r1) => SEQ(der(c,r1),ONE)
  case SMALLTIMES(r1, i) =>
    if(i==0)  ZERO else SEQ(der(c,r1), SMALLTIMES(r1,i-1))
  case LARGETIMES(r1, i) =>
    SEQ(der(c,r1),LARGETIMES(r1,i-1))
  case MIDDLETIMES(r1, j , i) =>
    if(i==0) ZERO else SEQ(der(c,r1), MIDDLETIMES(r1,j-1,i-1))
  case NOT(r1) =>
    NOT(der(c,r1))
  case RECD(_, r1) => der(c, r1)
  case CFUN(f) => if(f(c)) ONE else ZERO
}


// extracts a string from a value
def flatten(v: Val) : String = v match {
  case Empty => ""
  case Chr(c) => c.toString
  case charSet(c) => c.toString
  case Left(v) => flatten(v)
  case Right(v) => flatten(v)
  case Sequ(v1, v2) => flatten(v1) ++ flatten(v2)
  case Stars(vs) => vs.map(flatten).mkString
  case Rec(_, v) => flatten(v)
  case plus(vs) => vs.map(flatten).mkString
  case ntime(vs) => vs.map(flatten).mkString
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
  case plus(vs) => vs.flatMap(env)
  case charSet(s) => Nil
  case quMark(v) => env(v)
  case ntime(vs) => vs.flatMap(env)
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
  case MULTI(c) => Empty
  case CFUN(c) => Empty
  case ONEORMORE(r1) => mkeps(r1)
  //case MULTI(r1) => mkeps(r1)
  case ONEORO(r) => quMark(Empty)
  case NTIMES(r,n) => if(n==0) ntime(Nil) else ntime(mkeps(r)::Nil)
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
  case (ONEORMORE(r), Sequ(v1,Stars(vs))) => plus(inj(r,c,v1)::vs)
  //case (ONEORMORE(r),_) => plus(inj(r,c,v))
  case (MULTI(s), _) => charSet(c)
  case (ONEORO(r1),Sequ(v1,Empty)) => quMark(inj(r1,c,v1))
  case (NTIMES(r1,n),Sequ(v1,ntime(vs))) => ntime(inj(r1,c,v1)::vs)
  //case (NTIMES(r1,n),_) => Empty
  case (CFUN(x),_) => Chr(c)

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
def simp(r: Rexp): (Rexp, Val => Val) = r match {
  case ALT(r1, r2) => {
    val (r1s, f1s) = simp(r1)
    val (r2s, f2s) = simp(r2)
    (r1s, r2s) match {
      case (ZERO, _) => (r2s, F_RIGHT(f2s))
      case (_, ZERO) => (r1s, F_LEFT(f1s))
      case _ => if (r1s == r2s) (r1s, F_LEFT(f1s))
      else (ALT (r1s, r2s), F_ALT(f1s, f2s))
    }
  }
  case SEQ(r1, r2) => {
    val (r1s, f1s) = simp(r1)
    val (r2s, f2s) = simp(r2)
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
  {
    throw new Exception("lexing error") }
  case c::cs => {
    val (r_simp, f_simp) = simp(der(c, r))
    //println("r_sim "+r_simp)
    inj(r, c, f_simp(lex_simp(r_simp, cs)))
  }
}

def lexing_simp(r: Rexp, s: String) =
  env(lex_simp(r, s.toList))

def find_v(r:Rexp,s:List[Char]) : Val = s match{
  case Nil => if (nullable(r)) mkeps(r) else
  { throw new Exception("lexing error") }
  case c::cs => {
    val (r_simp, f_simp) = simp(der(c, r))
    //println("r_simp "+r_simp+"r: "+r+"f_simp "+f_simp)
    inj(r, c, f_simp(lex_simp(r_simp, cs)))
  }
}
// The Lexing Rules for the WHILE Language

def PLUS(r: Rexp) = r ~ r.%

def Range(s : List[Char]) : Rexp = s match {
  case Nil => ZERO
  case c::Nil => CHAR(c)
  case c::s => ALT(CHAR(c), Range(s))
}
def RANGE(s: String) = Range(s.toList)
abstract class Token
case object T_SEMI extends Token
case class T_PAREN(s: String) extends Token
case class T_ID(s: String) extends Token
case class T_OP(s: String) extends Token
case class T_NUM(n: Int) extends Token
case class T_KWD(s: String) extends Token
case class T_STR(s: String) extends Token
case class T_COMMENTS(str: String) extends Token

val token : PartialFunction[(String, String), Token] = {
  case ("s", _) => T_SEMI
  case ("p", s) => T_PAREN(s)
  case ("i", s) => T_ID(s)
  case ("o", s) => T_OP(s)
  case ("n", s) => T_NUM(s.toInt)
  case ("k", s) => T_KWD(s)
  case ("str", s) => T_STR(s)
  case("c",s) => T_COMMENTS(s)
}


// def keywords
val keywords : Rexp = "while" | "if" | "else" | "then" | "do" | "for" | "to" | "true" | "false" | "read" | "write" | "skip"
val operation : Rexp = "+" | "-" | "*" | "/" | "%" | "==" | "!=" | ">" | "<" | "<=" | ">=" | ":=" | "&&" | "||"
val letter: Rexp = MULTI(("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_").toList)
//val letter:Rexp = RANGE("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_")
val digit: Rexp = MULTI("0123456789".toList)
//val digit:Rexp = RANGE("0123456789")
val num: Rexp = ONEORMORE(digit)
val symbol : Rexp = letter | "." | "_" | ">" | "<" | "=" | ";" | "," | "\\" | ":"
val whitspaces: Rexp = ONEORMORE(" " | "\n" | "\t" | "\r")
val parenth: Rexp = "{" | "}" | "(" | ")"
val STRING: Rexp = "\"" ~ (digit|symbol|letter|whitspaces).% ~ "\""
val semicolons: Rexp = CHAR(';')
val id: Rexp = letter ~ STAR(letter | digit | "_")
val number: Rexp = "0" | (MULTI("123456789".toList) ~ STAR(digit))
val comments: Rexp = "//" ~ STAR(letter|digit|symbol|" "|"\t"|"\r") ~ "\n"

val WHILE_REGS = (("k" $ keywords) |
  ("i" $ id) |
  ("c" $ comments) |
  ("o" $ operation) |
  ("n" $ number) |
  ("s" $ semicolons) |
  ("str" $ STRING) |
  ("p" $ parenth) |
  ("w" $ whitspaces)
  ).%


// Two Simple While Tests
//========================

@arg(doc = "small tests")
@main
def small() = {

//  val prog0 = """333 + 222"""
//  println(s"test: $prog0")
//  println(WHILE_REGS)
//  println(lexing_simp(WHILE_REGS, prog0))
//
//  val prog1 = """read  n; write n"""
//  println(s"test: $prog1")
//  println(lexing_simp(WHILE_REGS, prog1))
  val reg = NTIMES(ALT(CHAR('a'),ONE),3)
  val reg2 = NTIMES(CHAR('a'),3)
  val reg4 = ONEORMORE(ALT(CHAR(' '),ONE))

  println(find_v(whitspaces,"  ".toList))
}

// Bigger Tests
//==============

// escapes strings and prints them out as "", "\n" and so on
def esc(raw: String): String = {
  import scala.reflect.runtime.universe._
  Literal(Constant(raw)).toString
}

def escape(tks: List[(String, String)]) =
  tks.map{ case (s1, s2) => (s1, esc(s2))}


// by using collect we filter out all unwanted tokens
def tokenise(s: String) : List[Token] =
  lexing_simp(WHILE_REGS, s).collect(token)





///
//
// runs with amm2 and amm3
///////////////parser////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////







case class ~[+A, +B](x: A, y: B)

// constraint for the input
type IsSeq[A] = A => Seq[_]


abstract class Parser[I : IsSeq, T]{
  def parse(in: I): Set[(T, I)]

  def parse_all(in: I) : Set[T] =
    for ((hd, tl) <- parse(in);
         if tl.isEmpty) yield hd
}

// parser combinators

// sequence parser
class SeqParser[I : IsSeq, T, S](p: => Parser[I, T],
                                 q: => Parser[I, S]) extends Parser[I, ~[T, S]] {
  def parse(in: I) =
    for ((hd1, tl1) <- p.parse(in);
         (hd2, tl2) <- q.parse(tl1)) yield (new ~(hd1, hd2), tl2)
}

// alternative parser
class AltParser[I : IsSeq, T](p: => Parser[I, T],
                              q: => Parser[I, T]) extends Parser[I, T] {
  def parse(in: I) = p.parse(in) ++ q.parse(in)
}

// map parser
class MapParser[I : IsSeq, T, S](p: => Parser[I, T],
                                 f: T => S) extends Parser[I, S] {
  def parse(in: I) = for ((hd, tl) <- p.parse(in)) yield (f(hd), tl)
}

// atomic parser for (particular) strings
case class StrParser(s: String) extends Parser[String, String] {
  def parse(sb: String) = {
    val (prefix, suffix) = sb.splitAt(s.length)
    if (prefix == s) Set((prefix, suffix)) else Set()
  }
}
//Token version op parser
case class T_OpParser(res: String) extends Parser[List[Token],String] {
  def parse(in : List[Token]) = if (in.isEmpty) Set() else in.head match {
    case T_OP(s) => if(s==res) Set((res,in.tail)) else Set()
    case T_KWD(s) => if(s==res) Set((res,in.tail)) else Set()
    case T_PAREN(s) => if(s==res) Set((res,in.tail)) else Set()
    case T_SEMI => Set((";",in.tail))
    case _ => Set()
  }
}

// atomic parser for identifiers (variable names)
case object IdParser extends Parser[String, String] {
  val reg = "[a-z][a-z,0-9]*".r
  def parse(sb: String) = reg.findPrefixOf(sb) match {
    case None => Set()
    case Some(s) => Set(sb.splitAt(s.length))
  }
}
//Token version id Parser
case object T_IdParser extends Parser[List[Token],String] {
  def parse(t: List[Token]) = t.head match {
    case T_ID(s) => Set((s,t.tail))
    case _ => Set()
  }
}
// atomic parser for numbers (transformed into ints)
case object NumParser extends Parser[String, Int] {
  val reg = "[0-9]+".r
  def parse(sb: String) = reg.findPrefixOf(sb) match {
    case None => Set()
    case Some(s) => {
      val (hd, tl) = sb.splitAt(s.length)
      Set((hd.toInt, tl))
    }
  }
}
//Token version str Parser
case object T_NumParser extends Parser[List[Token],Int]{
  def parse(in : List[Token]) = in.head match {
    case T_NUM(x) => Set((x,in.tail))
    case _ => Set()
  }

}
// the following string interpolation allows us to write
// StrParser(_some_string_) more conveniently as
//
// p"<_some_string_>"

implicit def parser_interpolation(sc: StringContext) = new {
  def p(args: Any*) = StrParser(sc.s(args:_*))
  def t(args: Any*) = T_OpParser(sc.s(args:_*))
}

// more convenient syntax for parser combinators
implicit def ParserOps[I : IsSeq, T](p: Parser[I, T]) = new {
  def ||(q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
  def map[S](f: => T => S) = new MapParser[I, T, S](p, f)
}



// the abstract syntax trees for the WHILE language
abstract class Stmt
abstract class AExp
abstract class BExp

type Block = List[Stmt]

case object Skip extends Stmt
case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
case class While(b: BExp, bl: Block) extends Stmt
case class Assign(s: String, a: AExp) extends Stmt
case class Write(s: String) extends Stmt

case class Var(s: String) extends AExp
case class Num(i: Int) extends AExp
case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

case object True extends BExp
case object False extends BExp
case class Bop(o: String, a1: AExp, a2: AExp) extends BExp
case class And(b1: BExp, b2: BExp) extends BExp
case class Or(b1: BExp, b2: BExp) extends BExp


// arithmetic expressions
lazy val AExp: Parser[String, AExp] =
  (Te ~ p"+" ~ AExp).map[AExp]{ case x ~ _ ~ z => Aop("+", x, z) } ||
    (Te ~ p"-" ~ AExp).map[AExp]{ case x ~ _ ~ z => Aop("-", x, z) } || Te
lazy val Te: Parser[String, AExp] =
  (Fa ~ p"*" ~ Te).map[AExp]{ case x ~ _ ~ z => Aop("*", x, z) } ||
    (Fa ~ p"/" ~ Te).map[AExp]{ case x ~ _ ~ z => Aop("/", x, z) } || Fa
lazy val Fa: Parser[String, AExp] =
  (p"(" ~ AExp ~ p")").map{ case _ ~ y ~ _ => y } ||
    IdParser.map(Var) ||
    NumParser.map(Num)

// Token version arithmetic expression
lazy val T_Aexp: Parser[List[Token], AExp] =
  (T_Te ~ t"+" ~ T_Aexp).map[AExp]{ case x ~ _ ~ z => Aop("+", x, z) } ||
    (T_Te ~ t"-" ~ T_Aexp).map[AExp]{ case x ~ _ ~ z => Aop("-", x, z) } || T_Te
lazy val T_Te: Parser[List[Token], AExp] =
  (T_Fa ~ t"*" ~ T_Te).map[AExp]{ case x ~ _ ~ z => Aop("*", x, z) } ||
    (T_Fa ~ t"/" ~ T_Te).map[AExp]{ case x ~ _ ~ z => Aop("/", x, z) } || T_Fa
lazy val T_Fa: Parser[List[Token], AExp] =
  (t"(" ~ T_Aexp ~ t")").map{ case _ ~ y ~ _ => y } ||
    T_IdParser.map(Var) ||
    T_NumParser.map(Num)
// boolean expressions with some simple nesting
lazy val BExp: Parser[String, BExp] =
  (AExp ~ p"==" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("==", x, z) } ||
    (AExp ~ p"!=" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("!=", x, z) } ||
    (AExp ~ p"<" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("<", x, z) } ||
    (AExp ~ p">" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop(">", x, z) } ||
    (p"(" ~ BExp ~ p")" ~ p"&&" ~ BExp).map[BExp]{ case _ ~ y ~ _ ~ _ ~ v => And(y, v) } ||
    (p"(" ~ BExp ~ p")" ~ p"||" ~ BExp).map[BExp]{ case _ ~ y ~ _ ~ _ ~ v => Or(y, v) } ||
    (p"true".map[BExp]{ _ => True }) ||
    (p"false".map[BExp]{ _ => False }) ||
    (p"(" ~ BExp ~ p")").map[BExp]{ case _ ~ x ~ _ => x }
//Tooken version boolean expression
lazy val T_BExp: Parser[List[Token], BExp] =
  (T_Aexp ~ t"==" ~ T_Aexp).map[BExp]{ case x ~ _ ~ z => Bop("==", x, z) } ||
    (T_Aexp ~ t"!=" ~ T_Aexp).map[BExp]{ case x ~ _ ~ z => Bop("!=", x, z) } ||
    (T_Aexp ~ t"<" ~ T_Aexp).map[BExp]{ case x ~ _ ~ z => Bop("<", x, z) } ||
    (T_Aexp ~ t">" ~ T_Aexp).map[BExp]{ case x ~ _ ~ z => Bop(">", x, z) } ||
    (t"(" ~ T_BExp ~ t")" ~ t"&&" ~ T_BExp).map[BExp]{ case _ ~ y ~ _ ~ _ ~ v => And(y, v) } ||
    (t"(" ~ T_BExp ~ t")" ~ t"||" ~ T_BExp).map[BExp]{ case _ ~ y ~ _ ~ _ ~ v => Or(y, v) } ||
    (t"true".map[BExp]{ _ => True }) ||
    (t"false".map[BExp]{ _ => False }) ||
    (t"(" ~ T_BExp ~ t")").map[BExp]{ case _ ~ x ~ _ => x }
// a single statement
lazy val Stmt: Parser[String, Stmt] =
  ((p"skip".map[Stmt]{_ => Skip }) ||
    (IdParser ~ p":=" ~ AExp).map[Stmt]{ case x ~ _ ~ z => Assign(x, z) } ||
    (p"write(" ~ IdParser ~ p")").map[Stmt]{ case _ ~ y ~ _ => Write(y) } ||
    (p"if" ~ BExp ~ p"then" ~ Block ~ p"else" ~ Block)
      .map[Stmt]{ case _ ~ y ~ _ ~ u ~ _ ~ w => If(y, u, w) } ||
    (p"while" ~ BExp ~ p"do" ~ Block).map[Stmt]{ case _ ~ y ~ _ ~ w => While(y, w) })
case class test_res(t:String,d:Token) extends Stmt
lazy val T_Stmt: Parser[List[Token],Stmt] =
  ((T_OpParser("skip").map[Stmt]{_ =>Skip}) ||
    (T_IdParser ~ T_OpParser(":=") ~ T_Aexp).map[Stmt]{ case x ~ _ ~ z => Assign(x,z)}||
    (t"write(" ~ T_IdParser ~ t")").map[Stmt]{ case _ ~ y ~ _ => Write(y) } ||
    (t"if" ~ T_BExp ~ t"then" ~ T_Block ~ t"else" ~ T_Block)
      .map[Stmt]{ case _ ~ y ~ _ ~ u ~ _ ~ w => If(y, u, w) } ||
    (t"while" ~ T_BExp ~ t"do" ~ T_Block).map[Stmt]{ case _ ~ y ~ _ ~ w => While(y, w) }
    )

// statements
lazy val Stmts: Parser[String, Block] =
  (Stmt ~ p";" ~ Stmts).map[Block]{ case x ~ _ ~ z => x :: z } ||
    (Stmt.map[Block]{ s => List(s) })
//token version statements
lazy val T_Stmts: Parser[List[Token],Block] =
  (T_Stmt ~ t";" ~ T_Stmts ).map[Block]{ case x ~ _ ~ z => x :: z } ||
    (T_Stmt.map[Block]{ s => List(s) })
// blocks (enclosed in curly braces)
lazy val Block: Parser[String, Block] =
  ((p"{" ~ Stmts ~ p"}").map{ case _ ~ y ~ _ => y } ||
    (Stmt.map(t => List(t))))
//Token version Block
lazy val T_Block: Parser[List[Token], Block] =
  ((t"{" ~ T_Stmts ~ t";" ~ t"}").map{ case _ ~ y ~ _ ~ _ => y } ||
    (T_Stmt.map(t => List(t))))

// Examples
println("////////////////////////////////////////////////////////////////////")
Stmt.parse_all("x2:=5+3")
Block.parse_all("{x:=5;y:=8}")
Block.parse_all("if(false)then{x:=5}else{x:=10}")


val fib = """n := 5;
             minus1 := 0;
             minus2 := 1;
             temp := 0;
             while (n > 0) do {
                 temp := minus2;
                 minus2 := minus1 + minus2;
                 minus1 := temp;
                 n := n - 1;
             };
             result := minus2""".replaceAll("\\s+", "")

Stmts.parse_all(fib)


// an interpreter for the WHILE language
type Env = Map[String, Int]

def eval_aexp(a: AExp, env: Env) : Int = a match {
  case Num(i) => i
  case Var(s) => env(s)
  case Aop("+", a1, a2) => eval_aexp(a1, env) + eval_aexp(a2, env)
  case Aop("-", a1, a2) => eval_aexp(a1, env) - eval_aexp(a2, env)
  case Aop("*", a1, a2) => eval_aexp(a1, env) * eval_aexp(a2, env)
  case Aop("/", a1, a2) => eval_aexp(a1, env) / eval_aexp(a2, env)
}

def eval_bexp(b: BExp, env: Env) : Boolean = b match {
  case True => true
  case False => false
  case Bop("==", a1, a2) => eval_aexp(a1, env) == eval_aexp(a2, env)
  case Bop("!=", a1, a2) => !(eval_aexp(a1, env) == eval_aexp(a2, env))
  case Bop(">", a1, a2) => eval_aexp(a1, env) > eval_aexp(a2, env)
  case Bop("<", a1, a2) => eval_aexp(a1, env) < eval_aexp(a2, env)
  case And(b1, b2) => eval_bexp(b1, env) && eval_bexp(b2, env)
  case Or(b1, b2) => eval_bexp(b1, env) || eval_bexp(b2, env)
}

def eval_stmt(s: Stmt, env: Env) : Env = s match {
  case Skip => env
  case Assign(x, a) => env + (x -> eval_aexp(a, env))
  case If(b, bl1, bl2) => if (eval_bexp(b, env)) eval_bl(bl1, env) else eval_bl(bl2, env)
  case While(b, bl) =>
    if (eval_bexp(b, env)) eval_stmt(While(b, bl), eval_bl(bl, env))
    else env
  case Write(x) => { println(env(x)) ; env }
}

def eval_bl(bl: Block, env: Env) : Env = bl match {
  case Nil => env
  case s::bl => eval_bl(bl, eval_stmt(s, env))
}
val test =
  """
    n := 5;
    i := 1;
    while(i<6) do{
    n:=n+n;
    i := i+1;
    }
    """.stripMargin.replaceAll("\\s+", "")
def eval(bl: Block) : Env = eval_bl(bl, Map())
println(" !!!!!!!!!!!!!!!"+eval(T_Stmts.parse_all(tokenise(fib)).head))