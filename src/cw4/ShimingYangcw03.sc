// Parser Combinators:
// Simple Version for WHILE-language
//====================================
//
// with some added convenience for
// map-parsers and grammar rules
//
// call with
//
//    amm ShimingYangcw03.sc test   for parsing all programs
//    amm ShimingYangcw03.sc test1  for interpreter of Fibonacci program
//    amm ShimingYangcw03.sc test2  for interpreter of The three‑nested‑loops program (start = 100)
//    amm ShimingYangcw03.sc test3  for interpreter of Prime number program
//    amm ShimingYangcw03.sc test4  for interpreter of Collatz series program





// load the token
import $file.token
import token._
import scala.io.StdIn

// more convenience for the map parsers later on;
// it allows writing nested patterns as
// case x ~ y ~ z => ...

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

case object TIdParser extends Parser[List[Token], String]{
    def parse(in:List[Token]) = if(in.isEmpty) Set() else in.head match {
      case T_ID(s:String) => Set((s,in.tail))
      case _ => Set()
    }
}

case object TLetParser extends Parser[List[Token], String]{
  def parse(in:List[Token]) =if(in.isEmpty) Set() else in.head match {
    case T_LETTER(s:String) => Set((s,in.tail))
    case _ => Set()
  }
}
case object TNumParser extends Parser[List[Token], Int]{
  def parse(in:List[Token]) = if(in.isEmpty) Set() else in.head match {
    case T_NUM(n:Int) => Set((n,in.tail))
    case _ => Set()
  }
}
case object TStrParser extends Parser[List[Token], String]{
  def parse(in:List[Token]) =if(in.isEmpty) Set() else in.head match {
    case T_STR(s:String) => Set((s,in.tail))
    case _ => Set()
  }
}






// more convenient syntax for parser combinators
implicit def ParserOps[I : IsSeq, T](p: Parser[I, T]) = new {
  def ||(q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
  def map[S](f: => T => S) = new MapParser[I, T, S](p, f)
}

case class TokenParser(t:List[Token]) extends Parser[List[Token],String]{
  override def parse(in:List[Token]) : Set[(String, List[Token])] = if(in.isEmpty) Set() else in.head match {
      case T_SEMI => if(t.head == in.head) Set((";",in.tail)) else Set()
      case T_PAREN(s:String) => if(t.head == in.head) Set((s,in.tail)) else Set()
      case T_ID(s:String) =>if(t.head == in.head) Set((s,in.tail)) else Set()
      case T_OP(s:String)=> if(t.head == in.head) Set((s,in.tail)) else Set()
      case T_NUM(n:Int) =>if(t.head == in.head) Set((n.toString,in.tail)) else Set()
      case T_KWD(s:String)=> if(t.head == in.head) Set((s,in.tail)) else Set()
      case T_STR(s:String) =>if(t.head == in.head) Set((s,in.tail)) else Set()
      case T_LETTER(s:String) =>if(t.head == in.head) Set((s,in.tail)) else Set()
      case T_SYM(s:String)=> if(t.head == in.head) Set((s,in.tail)) else Set()
      case _ => Set()


  }
}

  // the abstract syntax trees for the WHILE language
abstract class Stmt
abstract class AExp
abstract class BExp

type Block = List[Stmt]

case object Skip extends Stmt
case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
case class While(b: BExp, bl: Block) extends Stmt
case class ForLoop(s: String, a1:AExp,a2: AExp, bl: Block) extends Stmt
case class Assign(s: String, a: AExp) extends Stmt
case class Read(s: String) extends Stmt
case class WriteVar(s: String) extends Stmt
case class WriteStr(s: String) extends Stmt
//case class Write(s: String) extends Stmt

case class Var(s: String) extends AExp
case class Num(i: Int) extends AExp
case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

case object True extends BExp
case object False extends BExp
case class Bop(o: String, a1: AExp, a2: AExp) extends BExp
case class And(b1: BExp, b2: BExp) extends BExp
case class Or(b1: BExp, b2: BExp) extends BExp


// arithmetic expressions

lazy val AExp: Parser[List[Token], AExp] =
(Te ~ TokenParser(List(T_OP("+"))) ~ AExp).map[AExp]{ case x ~ _ ~ z => Aop("+", x, z) } ||
  (Te ~ TokenParser(List(T_OP("-"))) ~ AExp).map[AExp]{ case x ~ _ ~ z => Aop("-", x, z) } || Te
lazy val Te: Parser[List[Token], AExp] =
  (Fa ~ TokenParser(List(T_OP("*"))) ~ Te).map[AExp]{ case x ~ _ ~ z => Aop("*", x, z) } ||
    (Fa ~ TokenParser(List(T_OP("/"))) ~ Te).map[AExp]{ case x ~ _ ~ z => Aop("/", x, z) } ||
    (Fa ~ TokenParser(List(T_OP("%"))) ~ Te).map[AExp]{ case x ~ _ ~ z => Aop("%", x, z) } ||Fa
lazy val Fa: Parser[List[Token], AExp] =
  (TokenParser(List(T_PAREN("("))) ~ AExp ~ TokenParser(List(T_PAREN(")")))).map{ case _ ~ y ~ _ => y } ||
    TIdParser.map(Var) ||
    TLetParser.map(Var) ||
    TNumParser.map(Num)


// boolean expressions with some simple nesting
lazy val BExp: Parser[List[Token], BExp] =
  (AExp ~ TokenParser(List(T_OP("=="))) ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("==", x, z) } ||
    (AExp ~ TokenParser(List(T_OP("!="))) ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("!=", x, z) } ||
    (AExp ~ TokenParser(List(T_OP("<")))~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("<", x, z) } ||
    (AExp ~ TokenParser(List(T_OP(">")))~ AExp).map[BExp]{ case x ~ _ ~ z => Bop(">", x, z) } ||
    (AExp ~ TokenParser(List(T_OP("<=")))~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("<=", x, z) } ||
    (AExp ~ TokenParser(List(T_OP(">=")))~ AExp).map[BExp]{ case x ~ _ ~ z => Bop(">=", x, z) } ||
    (TokenParser(List(T_PAREN("("))) ~ BExp ~ TokenParser(List(T_PAREN(")"))) ~ TokenParser(List(T_OP("&&"))) ~ BExp).map[BExp]{ case _ ~ y ~ _ ~ _ ~ v => And(y, v) } ||
    (TokenParser(List(T_PAREN("(")))  ~ BExp ~ TokenParser(List(T_PAREN(")")))~ TokenParser(List(T_OP("||"))) ~ BExp).map[BExp]{ case _ ~ y ~ _ ~ _ ~ v => Or(y, v) } ||
    (TokenParser(List(T_KWD("true"))).map[BExp]{ _ => True }) ||
    (TokenParser(List(T_KWD("false"))).map[BExp]{ _ => False }) ||
    (TokenParser(List(T_PAREN("(")))  ~ BExp ~ TokenParser(List(T_PAREN(")")))).map[BExp]{ case _ ~ x ~ _ => x }

// a single statement
lazy val Stmt: Parser[List[Token], Stmt] =
  ((TokenParser(List(T_KWD("skip"))).map[Stmt]{_ => Skip }) ||
    (TIdParser  ~ TokenParser(List(T_OP(":="))) ~ AExp).map[Stmt]{ case x ~ _ ~ z => Assign(x, z) } ||
    (TLetParser ~ TokenParser(List(T_OP(":="))) ~ AExp).map[Stmt]{ case x ~ _ ~ z => Assign(x, z) } ||
    (TokenParser(List(T_KWD("write")))  ~ TokenParser(List(T_PAREN("(")))  ~ TIdParser ~ TokenParser(List(T_PAREN(")")))).map[Stmt]{ case x ~ _ ~ y ~ _  => WriteVar(y) } ||
    (TokenParser(List(T_KWD("write")))  ~ TIdParser ).map[Stmt]{ case x ~ y   => WriteVar(y) } ||
    (TokenParser(List(T_KWD("write")))  ~ TokenParser(List(T_PAREN("(")))  ~ TLetParser ~ TokenParser(List(T_PAREN(")")))).map[Stmt]{ case x ~ _ ~ y ~ _  => WriteVar(y) } ||
    (TokenParser(List(T_KWD("write")))  ~ TLetParser ).map[Stmt]{ case x ~ y   => WriteVar(y) } ||
    (TokenParser(List(T_KWD("write")))  ~ TStrParser).map[Stmt]{ case x ~  y   => WriteStr(y) } ||
    (TokenParser(List(T_KWD("read")))  ~ TokenParser(List(T_PAREN("(")))  ~ TIdParser ~ TokenParser(List(T_PAREN(")")))).map[Stmt]{ case x ~ _ ~ y ~ _  => Read(y) } ||
    (TokenParser(List(T_KWD("read")))  ~ TIdParser).map[Stmt]{ case x ~ y   => Read(y) } ||
    (TokenParser(List(T_KWD("read")))  ~ TokenParser(List(T_PAREN("(")))  ~ TLetParser ~ TokenParser(List(T_PAREN(")")))).map[Stmt]{ case x ~ _ ~ y ~ _  => Read(y) } ||
    (TokenParser(List(T_KWD("read")))  ~ TLetParser).map[Stmt]{ case x ~ y   => Read(y) } ||
    (TokenParser(List(T_KWD("if"))) ~ BExp ~ TokenParser(List(T_KWD("then"))) ~ Block ~ TokenParser(List(T_KWD("else"))) ~ Block)
      .map[Stmt]{ case _ ~ y ~ _ ~ u ~ _ ~ w => If(y, u, w) } ||
    (TokenParser(List(T_KWD("while"))) ~ BExp ~ TokenParser(List(T_KWD("do"))) ~ Block).map[Stmt]{ case _ ~ y ~ _ ~ w => While(y, w) } ||

    (TokenParser(List(T_KWD("for"))) ~ TIdParser  ~ TokenParser(List(T_OP(":="))) ~ AExp
      ~ TokenParser(List(T_KWD("upto"))) ~ AExp ~ TokenParser(List(T_KWD("do"))) ~ Block).map[Stmt]{ case _ ~ x ~ _ ~ y ~ _ ~ z ~ _ ~ m  => ForLoop(x, y,z,m) } ||
    (TokenParser(List(T_KWD("for"))) ~ TLetParser ~ TokenParser(List(T_OP(":="))) ~ AExp
      ~ TokenParser(List(T_KWD("upto"))) ~ AExp ~ TokenParser(List(T_KWD("do"))) ~ Block).map[Stmt]{ case _ ~ x ~ _ ~ y ~ _ ~ z ~ _ ~ m  => ForLoop(x, y ,z,m) }
    )


// statements
lazy val Stmts: Parser[List[Token], Block] =
  (Stmt ~ TokenParser(List(T_SEMI))~ Stmts).map[Block]{ case x ~ _ ~ z => x :: z } ||
    (Stmt.map[Block]{s => List(s)})

// blocks (enclosed in curly braces)
lazy val Block: Parser[List[Token], Block] =
  (( TokenParser(List(T_PAREN("{")))~ Stmts ~TokenParser(List(T_PAREN("}"))) ).map{ case _ ~ y ~ _ => y } ||
    (Stmt.map(s => List(s))))





@arg(doc = "Test parser")
@main
def test(): Unit ={
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
// prints out prime numbers from 2 to 100
end := 100;
n := 2;
while (n < end) do {
f := 2;
tmp := 0;
while ((f < n / 2 + 1) && (tmp == 0)) do {
if ((n / f) * f == n) then { tmp := 1 } else { skip };
f := f + 1
};
if (tmp == 0) then { write(n) } else { skip };
n := n + 1
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
  val prog5 =
    """for i := 2 upto 4 do {
      |write i
      |}""".stripMargin

  println("===================")
  val token1 = tokenise(prog1)
  println(Stmts.parse_all(token1))
  println("===================")

  println("===================")
  val token2 = tokenise(prog2)
  println(Stmts.parse_all(token2))
  println("===================")

  println("===================")
  val token3 = tokenise(prog3)
  println(Stmts.parse_all(token3))
  println("===================")

  println("===================")
  val token4 = tokenise(prog4)
  println(Stmts.parse_all(token4))
  println("===================")
  val token5 = tokenise(prog5)
  println(Stmts.parse_all(token5))
  println("===================")

}


// an interpreter for the WHILE language
type Env = Map[String, Int]

def eval_aexp(a: AExp, env: Env) : Int = a match {
  case Num(i) => i
  case Var(s) => env(s)
  case Aop("+", a1, a2) => eval_aexp(a1, env) + eval_aexp(a2, env)
  case Aop("-", a1, a2) => eval_aexp(a1, env) - eval_aexp(a2, env)
  case Aop("*", a1, a2) => eval_aexp(a1, env) * eval_aexp(a2, env)
  case Aop("/", a1, a2) => eval_aexp(a1, env) / eval_aexp(a2, env)
  case Aop("%", a1, a2) => eval_aexp(a1, env) % eval_aexp(a2, env)
}

def eval_bexp(b: BExp, env: Env) : Boolean = b match {
  case True => true
  case False => false
  case Bop("==", a1, a2) => eval_aexp(a1, env) == eval_aexp(a2, env)
  case Bop("!=", a1, a2) => !(eval_aexp(a1, env) == eval_aexp(a2, env))
  case Bop(">=", a1, a2) => eval_aexp(a1, env) >= eval_aexp(a2, env)
  case Bop("<=", a1, a2) => eval_aexp(a1, env) <= eval_aexp(a2, env)
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
  case ForLoop(s,a1,a2,bl) =>
    val env1 = eval_stmt(Assign(s,a1),env)
    val bl1 = bl :+ Assign(s,Aop("+",Var(s),Num(1)))
    if(eval_bexp(Bop("<=",Var(s),a2),env1)) eval_stmt(While(Bop("<=",Var(s),a2),bl1),eval_bl(bl1,env1))
    else env1


  case WriteVar(x) => { println(env(x)) ; env }
  case WriteStr(x) => { println(x) ; env }
  case Read(x) => {println("Please input a number...");env + (x ->StdIn.readInt())}
}

def eval_bl(bl: Block, env: Env) : Env = bl match {
  case Nil => env
  case s::bl => eval_bl(bl, eval_stmt(s, env))
}

def eval(bl: Block) : Env = eval_bl(bl, Map())



//test
@arg(doc = "Test 1")
@main
def test1(): Unit ={
  val factors =
    """write "Fib";
      |read n;
      |minus1 := 0;
      |minus2 := 1;
      |while n > 0 do {
      |temp := minus2;
      |minus2 := minus1 + minus2;
      |minus1 := temp;
      |n := n - 1
      |};
      |write "Result";
      |write minus2
      |""".stripMargin
  println("================================")
  println(eval(Stmts.parse_all(tokenise(factors)).head))
  println("================================")
}


//test
@arg(doc = "Test 2")
@main
def test2(): Unit = {
  val startTime: Long = System.currentTimeMillis
  val factors1 =
    """start := 1000;
      |x := start;
      |y := start;
      |z := start;
      |while 0 < x do {
      |while 0 < y do {
      |while 0 < z do { z := z - 1 };
      |z := start;
      |y := y - 1
      |};
      |y := start;
      |x := x - 1
      |}""".stripMargin
  println("================================")
  println(eval(Stmts.parse_all(tokenise(factors1)).head))
  val endTime: Long = System.currentTimeMillis
  System.out.println("the time consuming is " + (endTime - startTime) + "ms")
  println("================================")
}
//test
@arg(doc = "Test 3")
@main
def test3(): Unit = {
  val factors2 =
    """// prints out prime numbers from 2 to 100
      |end := 100;
      |n := 2;
      |while (n < end) do {
      |f := 2;
      |tmp := 0;
      |while ((f < n / 2 + 1) && (tmp == 0)) do {
      |if ((n / f) * f == n) then { tmp := 1 } else { skip };
      |f := f + 1
      |};
      |if (tmp == 0) then { write(n) } else { skip };
      |n := n + 1
      |}
      |""".stripMargin
  println("================================")
  println(eval(Stmts.parse_all(tokenise(factors2)).head))
  println("================================")
}
//test
@arg(doc = "Test 4")
@main
def test4(): Unit = {
  val factors3 =
    """// Collatz series
      |//
      |// needs writing of strings and numbers; comments
      |bnd := 1;
      |while bnd < 101 do {
      |write bnd;
      |write ": ";
      |n := bnd;
      |cnt := 0;
      |while n > 1 do {
      |write n;
      |write ",";
      |if n % 2 == 0
      |then n := n / 2
      |else n := 3 * n+1;
      |cnt := cnt + 1
      |};
      |write " => ";
      |write cnt;
      |write "\n";
      |bnd := bnd + 1
      |}
      |""".stripMargin
  println("================================")
  println(eval(Stmts.parse_all(tokenise(factors3)).head))
  println("================================")
}

@arg(doc = "Test 5")
@main
def test5(): Unit = {
  val factors4 =
    """for i := 1 upto 10 do {
      |for i := 1 upto 10 do {
      |write i
      |}
      |}
      |""".stripMargin
  println("================================")
  println(eval(Stmts.parse_all(tokenise(factors4)).head))
  println("================================")
}







