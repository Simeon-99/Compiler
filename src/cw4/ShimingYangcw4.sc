// A Small Compiler for the WHILE Language
// (it does not use a parser nor lexer)
//
// cal with
//
//   amm ShimingYangcw4.sc



import $file.ShimingYangcw03
import ShimingYangcw03._
import $file.token
import token._



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

.method public static writes(Ljava/lang/String;)V
    .limit stack 2
    .limit locals 1
    getstatic java/lang/System/out Ljava/io/PrintStream;
    aload 0
    invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
    return
.end method


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
    ldc 13   ; the newline delimiter
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
  case "/" => i"idiv"
  case "%" => i"irem"

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
  case Bop(">", a1, a2) =>
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmple $jmp"
  case Bop("<=", a1, a2) =>
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpgt $jmp"
  case Bop(">=", a1, a2) =>
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmplt $jmp"
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
  case ForLoop(s,a1,a2,bl) =>{
    val (instrs1, env1) = compile_stmt(Assign(s,a1), env)

    val loop_begin = Fresh("Loop_begin")
    val loop_end = Fresh("Loop_end")
    val (instrs2, env2) = compile_block(bl, env1)
    val (instrs3, env3) = compile_stmt(Assign(s,Aop("+",Var(s),Num(1))),env2)

    (instrs1 ++
      l"$loop_begin" ++
      compile_bexp(Bop("<=",Var(s),a2), env1, loop_end) ++
      instrs2 ++
      instrs3 ++
      i"goto $loop_begin" ++
      l"$loop_end", env3)

  }
  case WriteStr(x) =>
    (i"ldc $x \t\t; $x" ++
      i"invokestatic XXX/XXX/writes(Ljava/lang/String;)V", env)

  case WriteVar(x) =>
    (i"iload ${env(x)} \t\t; $x" ++
      i"invokestatic XXX/XXX/write(I)V", env)
  case Read(x) => {
    val index = env.getOrElse(x, env.keys.size)
    (i"invokestatic XXX/XXX/read()I" ++
     i"istore $index \t\t; $x", env + (x -> index))
  }
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



// compiling and running .j-files
//
// JVM files can be assembled with
//
//    java -jar jasmin.jar fib.j
//
// and started with
//
//    java fib/fib



@main
def test() ={
  val prog11:String = {
    """for i := 2 upto 4 do {
      |write i
      |}
      |""".stripMargin
  }

    val token1 = tokenise(prog11)
    val blocks1 = (Stmts.parse_all(token1)).head
    println(compile(blocks1, "forloops"))
//  val prog2 = {
//    """
//      |write "Factorial, Please input a number...";
//      |read n;
//      |fact := 1;
//      |i := 1;
//      |while i <= n do {
//      |fact := fact * i;
//      |i := i + 1
//      |};
//      |write "Result";
//      |write fact
//      |""".stripMargin
//  }
//  val token2 = tokenise(prog2)
//  val blocks2 = (Stmts.parse_all(token2)).head
//  println(compile(blocks2, "fact"))
}

// runs with amm2 and amm3
