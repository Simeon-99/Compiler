// A Small Compiler for a Simple Functional Language
// (includes a parser and lexer)
//
// call with
//
//    amm fun.sc test
//
//  or
//
//    amm fun.sc main defs.fun
//    amm fun.sc main fact.fun
//
//  or
// 
//    amm fun.sc run defs.fun
//    amm fun.sc run fact.fun   
//
// the first prints out the JVM instructions for two
// factorial functions
//
// the next compile/run fun files
//

import $file.fun_tokens, fun_tokens._
import $file.fun_parser, fun_parser._ 


// calculating the maximal needed stack size
def max_stack_exp(e: Exp): Int = e match {
  case Call(_, args) => args.map(max_stack_exp).sum
  case If(a, e1, e2) => 
    max_stack_bexp(a) + (List(max_stack_exp(e1), max_stack_exp(e2)).max)
  case Write(e) => max_stack_exp(e) + 1
  case Var(_) => 1
  case Num(_) => 1
  case Aop(_, a1, a2) => max_stack_exp(a1) + max_stack_exp(a2)
  case Sequence(e1, e2) => List(max_stack_exp(e1), max_stack_exp(e2)).max
}

def max_stack_bexp(e: BExp): Int = e match {
  case Bop(_, a1, a2) => max_stack_exp(a1) + max_stack_exp(a2)
}

// compiler - built-in functions 
// copied from http://www.ceng.metu.edu.tr/courses/ceng444/link/jvm-cpm.html
//

val library = """
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

"""

// for generating new labels
var counter = -1

def Fresh(x: String) = {
  counter += 1
  x ++ "_" ++ counter.toString()
}

// convenient string interpolations for
// generating instructions, labels etc
import scala.language.implicitConversions
import scala.language.reflectiveCalls

// convenience for code-generation (string interpolations)
implicit def sring_inters(sc: StringContext) = new {
  def i(args: Any*): String = "   " ++ sc.s(args:_*) ++ "\n"  // instructions
  def l(args: Any*): String = sc.s(args:_*) ++ ":\n"          // labels
  def m(args: Any*): String = sc.s(args:_*) ++ "\n"           // methods
}

// variable / index environments
type Env = Map[String, Int]

def compile_op(op: String) = op match {
  case "+" => i"iadd"
  case "-" => i"isub"
  case "*" => i"imul"
  case "/" => i"idiv"
  case "%" => i"irem"
}


// compile expressions
def compile_exp(a: Exp, env: Env) : String = a match {
  case Num(i) => i"ldc $i"
  case Var(s) => i"iload ${env(s)}"
  case Aop(op, a1, a2) => 
    compile_exp(a1, env) ++ compile_exp(a2, env) ++ compile_op(op)
  case If(b, a1, a2) => {
    val if_else = Fresh("If_else")
    val if_end = Fresh("If_end")
    compile_bexp(b, env, if_else) ++
    compile_exp(a1, env) ++
    i"goto $if_end" ++
    l"$if_else" ++
    compile_exp(a2, env) ++
    l"$if_end"
  }
  case Call(name, args) => {
    val is = "I" * args.length
    args.map(a => compile_exp(a, env)).mkString ++
    i"invokestatic XXX/XXX/$name($is)I"
  }
  case Sequence(a1, a2) => {
    compile_exp(a1, env) ++ i"pop" ++ compile_exp(a2, env)
  }
  case Write(a1) => {
    compile_exp(a1, env) ++
    i"dup" ++
    i"invokestatic XXX/XXX/write(I)V"
  }
}

// compile boolean expressions
def compile_bexp(b: BExp, env : Env, jmp: String) : String = b match {
  case Bop("==", a1, a2) => 
    compile_exp(a1, env) ++ compile_exp(a2, env) ++ i"if_icmpne $jmp"
  case Bop("!=", a1, a2) => 
    compile_exp(a1, env) ++ compile_exp(a2, env) ++ i"if_icmpeq $jmp"
  case Bop("<", a1, a2) => 
    compile_exp(a1, env) ++ compile_exp(a2, env) ++ i"if_icmpge $jmp"
  case Bop("<=", a1, a2) => 
    compile_exp(a1, env) ++ compile_exp(a2, env) ++ i"if_icmpgt $jmp"
}

// compile functions and declarations
def compile_decl(d: Decl) : String = d match {
  case Def(name, args, a) => { 
    val env = args.zipWithIndex.toMap
    val is = "I" * args.length
    m".method public static $name($is)I" ++
    m".limit locals ${args.length}" ++
    m".limit stack ${1 + max_stack_exp(a)}" ++
    l"${name}_Start" ++   
    compile_exp(a, env) ++
    i"ireturn" ++
    m".end method\n"
  }
  case Main(a) => {
    m".method public static main([Ljava/lang/String;)V" ++
    m".limit locals 1" ++
    m".limit stack ${1 + max_stack_exp(a)}" ++
    compile_exp(a, Map()) ++
    i"invokestatic XXX/XXX/write(I)V" ++
    i"return" ++
    m".end method\n"
  }
}

// the main compilation function
def compile(prog: List[Decl], class_name: String) : String = {
  val instructions = prog.map(compile_decl).mkString
  (library + instructions).replaceAllLiterally("XXX", class_name)
}


import ammonite.ops._

def compile_to_file(prog: List[Decl], class_name: String) : Unit = 
  write.over(pwd / s"$class_name.j", compile(prog, class_name))  

def compile_and_run(prog: List[Decl], class_name: String) : Unit = {
  println(s"Start of compilation")
  compile_to_file(prog, class_name)
  println(s"generated $class_name.j file")
  os.proc("java", "-jar", "jasmin.jar", s"$class_name.j").call()
  println(s"generated $class_name.class file")
  println(s"Run program")
  os.proc("java", s"${class_name}/${class_name}").call(stdout = os.Inherit)
  println(s"done.")
}


// An example program (two versions of factorial)
//
// def fact(n) = 
//    if n == 0 then 1 else n * fact(n - 1);
//
// def facT(n, acc) = 
//    if n == 0 then acc else facT(n - 1, n * acc);
// 
// fact(10) ; facT(10, 1)
// 


val test_prog = 
  List(Def("fact", List("n"),
         If(Bop("==",Var("n"),Num(0)),
            Num(1),
            Aop("*",Var("n"),
                    Call("fact",List(Aop("-",Var("n"),Num(1))))))),

       Def("facT",List("n", "acc"),
         If(Bop("==",Var("n"),Num(0)),
            Var("acc"),
            Call("facT",List(Aop("-",Var("n"),Num(1)), 
                             Aop("*",Var("n"),Var("acc")))))),

       Main(Sequence(Write(Call("fact",List(Num(10)))),
                     Write(Call("facT",List(Num(10), Num(1)))))))

// prints out the JVM instructions for the factorial example
@main
def test() = 
  println(compile(test_prog, "fact"))


@main
def main(fname: String) = {
    val path = os.pwd / fname
    val class_name = fname.stripSuffix("." ++ path.ext)
    val tks = tokenise(os.read(path))
    val ast = parse_tks(tks)
    println(compile(ast, class_name))
}

@main
def run(fname: String) = {
    val path = os.pwd / fname
    val class_name = fname.stripSuffix("." ++ path.ext)
    val tks = tokenise(os.read(path))
    val ast = parse_tks(tks)
    compile_and_run(ast, class_name)
}
