// A Small Compiler for the WHILE Language
//
// - this compiler contains support for "static" integer 
//   arrays (they are mutable but cannot be re-sized)
//
// Call with 
//
// amm compile_arrays.sc
  

// the abstract syntax trees for WHILE

abstract class Stmt
abstract class AExp
abstract class BExp 
type Block = List[Stmt]

// statements
case object Skip extends Stmt
case class ArrayDef(s: String, n: Int) extends Stmt            // array definition
case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
case class While(b: BExp, bl: Block) extends Stmt
case class Assign(s: String, a: AExp) extends Stmt             // var := exp
case class AssignA(s: String, a1: AExp, a2: AExp) extends Stmt // arr[exp1] := exp2
case class Write(s: String) extends Stmt


// arithmetic expressions
case class Var(s: String) extends AExp
case class Num(i: Int) extends AExp
case class Aop(o: String, a1: AExp, a2: AExp) extends AExp
case class Ref(s: String, a: AExp) extends AExp

// boolean expressions
case object True extends BExp
case object False extends BExp
case class Bop(o: String, a1: AExp, a2: AExp) extends BExp


// compiler headers needed for the JVM
//
// - contains a main method and a method for writing out an integer
//
// - the stack and locals are hard-coded
//

val beginning = """
.class public XXX.XXX
.super java/lang/Object

.method public static write(I)V 
    .limit locals 1 
    .limit stack 2 
    getstatic java/lang/System/out Ljava/io/PrintStream; 
    iload 0
    invokevirtual java/io/PrintStream/print(I)V
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



// for generating new labels
var counter = -1

def Fresh(x: String) = {
  counter += 1
  x ++ "_" ++ counter.toString()
}

// environments for variables and indices
type Env = Map[String, Int]

// convenient string interpolations 
// for generating instructions and labels

implicit def sring_inters(sc: StringContext) = new {
    def i(args: Any*): String = "   " ++ sc.s(args:_*) ++ "\n"
    def l(args: Any*): String = sc.s(args:_*) ++ ":\n"
}

def compile_op(op: String) = op match {
  case "+" => i"iadd"
  case "-" => i"isub"
  case "*" => i"imul"
}

// arithmetic expression compilation
def compile_aexp(a: AExp, env : Env) : String = a match {
  case Num(i) => i"ldc $i"
  case Var(s) => i"iload ${env(s)}"
  case Aop(op, a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ compile_op(op)
  case Ref(s, a) =>
    i"aload ${env(s)}" ++ compile_aexp(a, env) ++  i"iaload"
}

// boolean expression compilation
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
  case ArrayDef(s: String, n: Int) => {
    val index = if (env.isDefinedAt(s)) throw new Exception("array def error") else 
                    env.keys.size
    (i"ldc $n" ++
     i"newarray int" ++
     i"astore $index", env + (s -> index))
  }
  case AssignA(s, a1, a2) => {
    val index = if (env.isDefinedAt(s)) env(s) else 
                    throw new Exception("array not defined")
    (i"aload ${env(s)}" ++
     compile_aexp(a1, env) ++
     compile_aexp(a2, env) ++
     i"iastore", env)
  } 
}

// compile a block (i.e. list of statements)
def compile_block(bl: Block, env: Env) : (String, Env) = bl match {
  case Nil => ("", env)
  case s::bl => {
    val (instrs1, env1) = compile_stmt(s, env)
    val (instrs2, env2) = compile_block(bl, env1)
    (instrs1 ++ instrs2, env2)
  }
}


// main compile function for blocks (adds headers and proper JVM names)
def compile(bl: Block, class_name: String) : String = {
  val instructions = compile_block(bl, Map())._1
  (beginning ++ instructions ++ ending).replace("XXX", class_name)
}



// contrived example involving arrays
val array_test = 
  List(ArrayDef("a", 10),               // array a[10]
       ArrayDef("b", 2),                // array b[2]
       AssignA("a", Num(0), Num(10)),   // a[0] := 10
       Assign("x", Ref("a", Num(0))),   // x := a[0]
       Write("x"),            
       AssignA("b", Num(1), Num(5)),    // b[1] := 5
       Assign("x", Ref("b", Num(1))),   // x := b[1] 
       Write("x"))                     


// prints out the JVM-assembly instructions for fib above
//
//    println(compile(array_test, "arr"))
//
// can be assembled by hand with 
//
//    java -jar jasmin.jar arr.j
//
// and run with
//
//    java arr/arr

// automating the above
import ammonite.ops._

def compile_to_file(bl: Block, class_name: String) : Unit = 
  write.over(pwd / s"$class_name.j", compile(bl, class_name))  

def compile_and_run(bl: Block, class_name: String) : Unit = {
  println(s"Start of compilation")
  compile_to_file(bl, class_name)
  println(s"generated $class_name.j file")
  os.proc("java", "-jar", "jasmin.jar", s"$class_name.j").call()
  println(s"generated $class_name.class file ")
  //println(os.proc("java", s"${class_name}/${class_name}").call().out.text())
  os.proc("java", s"${class_name}/${class_name}").call(stdout = os.Inherit)
  println(s"done.")
}


   
@main def main() = {
  compile_and_run(array_test, "arr")
}




// runs with amm2 and amm3

