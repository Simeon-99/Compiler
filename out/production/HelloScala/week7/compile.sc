// A Small Compiler for the WHILE Language
// (it does not use a parser nor lexer)
//
// cal with 
//
//   amm compile.sc test
//   amm compile.sc test2


// the abstract syntax trees
abstract class Stmt
abstract class AExp
abstract class BExp 
type Block = List[Stmt]

// statements
case object Skip extends Stmt
case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
case class While(b: BExp, bl: Block) extends Stmt
case class Assign(s: String, a: AExp) extends Stmt
case class Write(s: String) extends Stmt
case class Read(s: String) extends Stmt

// arithmetic expressions
case class Var(s: String) extends AExp
case class Num(i: Int) extends AExp
case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

// boolean expressions
case object True extends BExp
case object False extends BExp
case class Bop(o: String, a1: AExp, a2: AExp) extends BExp


// compiler headers needed for the JVM
// (contains methods for read and write)
val beginning = """
.class public XXX.XXX
.super java/lang/Object

.method public static write(I)V 
    .limit locals 1 
    .limit stack 2 
    getstatic java/lang/System/out Ljava/io/PrintStream; 
    iload 0
    invokevirtual java/io/PrintStream/println(I)V 
    return 
.end method

.method public static main([Ljava/lang/String;)V
   .limit locals 200
   .limit stack 200

; COMPILED CODE STARTS

"""

val ending = """
; COMPILED CODE ENDS
   return

.end method
"""

// Compiler functions


// for generating new labels
var counter = -1

def Fresh(x: String) = {
  counter += 1
  x ++ "_" ++ counter.toString()
}

// convenient string interpolations 
// for instructions and labels
import scala.language.implicitConversions
import scala.language.reflectiveCalls

implicit def sring_inters(sc: StringContext) = new {
    def i(args: Any*): String = "   " ++ sc.s(args:_*) ++ "\n"
    def l(args: Any*): String = sc.s(args:_*) ++ ":\n"
}

// this allows us to write things like
// i"iadd" and l"Label"


// environments 
type Env = Map[String, Int]


def compile_op(op: String) = op match {
  case "+" => i"iadd"
  case "-" => i"isub"
  case "*" => i"imul"
}

// arithmetic expression compilation
def compile_aexp(a: AExp, env : Env) : String = a match {
  case Num(i) => i"ldc $i"
  case Var(s) => i"iload ${env(s)} \t\t; $s"
  case Aop(op, a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ compile_op(op)
}

// boolean expression compilation
//  - the jump-label is for where to jump if the condition is not true
def compile_bexp(b: BExp, env : Env, jmp: String) : String = b match {
  case True => ""
  case False => i"goto $jmp"
  case Bop("==", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpne $jmp"
  case Bop("!=", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpeq $jmp"
  case Bop("<", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpge $jmp"
}

// statement compilation
def compile_stmt(s: Stmt, env: Env) : (String, Env) = s match {
  case Skip => ("", env)
  case Assign(x, a) => {
    val index = env.getOrElse(x, env.keys.size)
    (compile_aexp(a, env) ++ i"istore $index \t\t; $x", env + (x -> index))
  } 
  case If(b, bl1, bl2) => {
    val if_else = Fresh("If_else")
    val if_end = Fresh("If_end")
    val (instrs1, env1) = compile_block(bl1, env)
    val (instrs2, env2) = compile_block(bl2, env1)
    (compile_bexp(b, env, if_else) ++
     instrs1 ++
     i"goto $if_end" ++
     l"$if_else" ++
     instrs2 ++
     l"$if_end", env2)
  }
  case While(b, bl) => {
    val loop_begin = Fresh("Loop_begin")
    val loop_end = Fresh("Loop_end")
    val (instrs1, env1) = compile_block(bl, env)
    (l"$loop_begin" ++
     compile_bexp(b, env, loop_end) ++
     instrs1 ++
     i"goto $loop_begin" ++
     l"$loop_end", env1)
  }
  case Write(x) => 
    (i"iload ${env(x)} \t\t; $x" ++ 
     i"invokestatic XXX/XXX/write(I)V", env)
  //case Read(x) => {
  //  val index = env.getOrElse(x, env.keys.size) 
  //  (i"invokestatic XXX/XXX/read()I" ++ 
  //   i"istore $index \t\t; $x", env + (x -> index))
  //}
}

// compilation of a block (i.e. list of instructions)
def compile_block(bl: Block, env: Env) : (String, Env) = bl match {
  case Nil => ("", env)
  case s::bl => {
    val (instrs1, env1) = compile_stmt(s, env)
    val (instrs2, env2) = compile_block(bl, env1)
    (instrs1 ++ instrs2, env2)
  }
}

// main compilation function for blocks
def compile(bl: Block, class_name: String) : String = {
  val instructions = compile_block(bl, Map.empty)._1
  (beginning ++ instructions ++ ending).replaceAllLiterally("XXX", class_name)
}





// Fibonacci numbers as a bare-bone test-case
val fib_test = 
  List(Assign("n", Num(9)),            //  n := 9;                     
       Assign("minus1",Num(0)),         //  minus1 := 0;
       Assign("minus2",Num(1)),         //  minus2 := 1;
       Assign("temp",Num(0)),           //  temp := 0;
       While(Bop("<",Num(0),Var("n")),  //  while 0 < n do  {
          List(Assign("temp",Var("minus2")), //  temp := minus2;
               Assign("minus2",Aop("+",Var("minus1"),Var("minus2"))), 
                                        //  minus2 := minus1 + minus2;
               Assign("minus1",Var("temp")), //  minus1 := temp;
               Assign("n",Aop("-",Var("n"),Num(1))))), //  n := n - 1 };
       Write("minus1"))                 //  write minus1



// prints out the JVM instructions
@main
def test() = 
  println(compile(fib_test, "fib"))





// compiling and running .j-files
//
// JVM files can be assembled with 
//
//    java -jar jasmin.jar fib.j
//
// and started with
//
//    java fib/fib


def run(bl: Block, class_name: String) = {
    val code = compile(bl, class_name)
    os.write.over(os.pwd / s"$class_name.j", code)
    os.proc("java", "-jar", "jasmin.jar", s"$class_name.j").call()
    os.proc("java", s"$class_name/$class_name").call(stdout = os.Inherit, stdin = os.Inherit)
}


@main
def test2() =
  run(fib_test, "fib")


/* Jasmin code for reading an integer

.method public static read()I 
    .limit locals 10 
    .limit stack 10

    ldc 0 
    istore 1  ; this will hold our final integer 
Label1: 
    getstatic java/lang/System/in Ljava/io/InputStream; 
    invokevirtual java/io/InputStream/read()I 
    istore 2 
    iload 2 
    ldc 10   ; the newline delimiter 
    isub 
    ifeq Label2 
    iload 2 
    ldc 32   ; the space delimiter 
    isub 
    ifeq Label2

    iload 2 
    ldc 48   ; we have our digit in ASCII, have to subtract it from 48 
    isub 
    ldc 10 
    iload 1 
    imul 
    iadd 
    istore 1 
    goto Label1 
Label2: 
    ; when we come here we have our integer computed in local variable 1 
    iload 1 
    ireturn 
.end method

*/

@main
def test3() ={
  val ifb = List(Assign("x",Num(3)),Assign("b",Num(3)))
  val elb = List(Assign("z",Num(5)))
  val bo = Bop("==",Var("x"),Num(0))
  val res = compile_stmt(If(bo,ifb,elb),Map("x" -> 0))
  println(res._2)
}



// runs with amm2 and amm3
