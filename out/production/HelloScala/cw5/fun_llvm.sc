// A Small LLVM Compiler for a Simple Functional Language
// (includes an external lexer and parser)
//
//
// call with 
//
//     amm fun_llvm.sc main fact.fun
//     amm fun_llvm.sc main defs.fun
//     .....
//
// or
//
//     amm fun_llvm.sc write fact.fun
//     amm fun_llvm.sc write defs.fun
//     .....
//
//     this will generate an .ll file.
//
//
// You can interpret an .ll file using lli, for example
//
//      lli fact.ll
//



import $file.parser, parser._
import $file.token, token._
import $file.lexer, lexer._

type Ty = String
type TyEnv = Map[String, Ty]

var tyenv :TyEnv =
  Map("new_line"->"Void","print_star"->"Void","print_char"->"Void",
    "print_space"->"Void","skip"->"Void","print_int"->"Void")

def typ_val(v: KVal, ts: TyEnv):Ty = v match {
  case KVar(s,_) => ts(s)
  case KNum(i) => "Int"
  case KFNum(f) => "Double"
  case Kop(o, v1, v2) => typ_val(v1,ts)
  case KCConst(o) => "Int"
  case KCFConst(o) => "Double"
  case KCall(name,vrs) => ts(name)
  case _ => "UNDEF"
}

def typ_exp(a: KExp, ts: TyEnv):Ty = a match {
  case KLet(x,e1,e2) => typ_val(e1,ts)
  case KIf(x1, e1, e2) => typ_exp(e1,ts)
  case _ => "UNDEF"
}

// for generating new labels
var counter = -1

def Fresh(x: String) = {
  counter += 1
  x ++ "_" ++ counter.toString()
}

// Internal CPS language for FUN
abstract class KExp
abstract class KVal

case class KVar(s: String , ty: Ty = "UNDEF") extends KVal
case class KNum(i: Int) extends KVal
case class KFNum(f: Double) extends KVal
case class KChConst(c: Int) extends KVal
case class Kop(o: String, v1: KVal, v2: KVal) extends KVal
case class KCConst(o :String) extends KVal
case class KCFConst(o :String) extends KVal
case class KCall(o: String, vrs: List[KVal]) extends KVal
case class KWrite(v: KVal) extends KVal

case class KLet(x: String, e1: KVal, e2: KExp) extends KExp {
  override def toString = s"LET $x = $e1 in \n$e2" 
}
case class KIf(x1: String, e1: KExp, e2: KExp) extends KExp {
  def pad(e: KExp) = e.toString.replaceAll("(?m)^", "  ")

  override def toString = 
     s"IF $x1\nTHEN\n${pad(e1)}\nELSE\n${pad(e2)}"
}
case class KReturn(v: KVal) extends KExp


// CPS translation from Exps to KExps using a
// continuation k.
def CPS(e: Exp)(k: KVal => KExp) : KExp = e match {
  case Var(s) => {
    if(s.head.isUpper){
      typ_val(KVar(s),tyenv) match {
        case "Int" =>{
          val z = Fresh("tmp")
          tyenv += (z -> "Int")
          KLet(z,KCConst(s),k(KVar(z,"Int")))
        }
        case "Double" =>{
          val z = Fresh("tmp")
          tyenv += (z -> "Double")
          KLet(z,KCFConst(s),k(KVar(z,"Double")))
        }
      }
    }else{
      k(KVar(s,typ_val(KVar(s),tyenv)))
    }
  }
  case Num(i) => k(KNum(i))
  case FNum(i) => k(KFNum(i))
  case ChConst(c) => k(KChConst(c))

  case Aop(o, e1, e2) => {
    val z = Fresh("tmp")
    CPS(e1)(y1 => 
      CPS(e2)(y2 =>  {
        tyenv += (z -> typ_val(y1,tyenv))
        KLet(z, Kop(o, y1, y2), k(KVar(z,typ_val(y1,tyenv))))
      }
      ))
  }
  case If(Bop(o, b1, b2), e1, e2) => {
    val z = Fresh("tmp")
    CPS(b1)(y1 => 
      CPS(b2)(y2 => {
        tyenv += (z -> "i1")
        KLet(z, Kop(o, y1, y2), KIf(z, CPS(e1)(k), CPS(e2)(k)))
      }
      ))
  }
  case Call(name, args) => {
    def aux(args: List[Exp], vs: List[KVal]) : KExp = args match {
      case Nil => {
        typ_val(KVar(name),tyenv) match {
          case "Int" =>{
            val z = Fresh("tmp")
            tyenv += (z -> "Int")
            KLet(z, KCall(name, vs), k(KVar(z,"Int")))
          }
          case "Double" =>{
            val z = Fresh("tmp")
            tyenv += (z -> "Double")
            KLet(z, KCall(name, vs), k(KVar(z,"Double")))
          }
          case "Void" => {
            val z = Fresh("tmp")
            tyenv += (z -> "Void")
            KLet(z, KCall(name, vs), k(KVar(z,"Void")))
          }
        }
      }
      case e::es => CPS(e)(y => aux(es, vs ::: List(y)))
    }
    aux(args, Nil)
  }
  case Sequence(e1, e2) => 
    CPS(e1)(_ => CPS(e2)(y2 => k(y2)))

}   

//initial continuation
def CPSi(e: Exp) = CPS(e)(KReturn)


// convenient string interpolations 
// for instructions, labels and methods
import scala.language.{implicitConversions, reflectiveCalls}

implicit def sring_inters(sc: StringContext) = new {
    def i(args: Any*): String = "   " ++ sc.s(args:_*) ++ "\n"
    def l(args: Any*): String = sc.s(args:_*) ++ ":\n"
    def m(args: Any*): String = sc.s(args:_*) ++ "\n"
}

// mathematical and boolean operations
def compile_op(op: String) = op match {
  case "+" => "add i32 "
  case "*" => "mul i32 "
  case "-" => "sub i32 "
  case "/" => "sdiv i32 "
  case "%" => "srem i32 "
  case "==" => "icmp eq i32 "
  case "!=" => "icmp ne i32 "
  case "<=" => "icmp sle i32 "     // signed less or equal
  case "<"  => "icmp slt i32 "     // signed less than
}

def compile_dop(op: String) = op match {
  case "+" => "fadd double "
  case "*" => "fmul double "
  case "-" => "fsub double "
  case "==" => "fcmp oeq double "
  case "!=" => "fcmp one double "
  case "<=" => "fcmp ole double "
  case "<" => "fcmp olt double "
}

// compile K values
def compile_val(v: KVal) : String = v match {
  case KNum(i) => s"${i.toString}"
  case KVar(s,_) => s"%$s"
  case KFNum(f) => s"${f.toString}"
  case KChConst(c) => s"${c.toString}"
  case KCConst(s) => s"load i32, i32* @$s"
  case KCFConst(s) => s"load double, double* @$s"

  case Kop(op, x1, x2) => {
    typ_val(x1,tyenv) match {
      case "Double" =>
        s"${compile_dop(op)} ${compile_val(x1)}, ${compile_val(x2)}"
      case _ =>
        s"${compile_op(op)} ${compile_val(x1)}, ${compile_val(x2)}"
    }
  }

  case KCall(x1, args) => {
    var str = ""
    for(x <- args){
      x match{
        case KVar(s,_) => {
          typ_val(KVar(s),tyenv) match {
            case "Int" => str += "i32 %"+ s+ ", "
            case "Double" => str += "double %"+ s+ ", "
            case _ => str += "i32 %"+s + ", "
          }
        }
        case KNum(i) => str += "i32 "+ i + ", "
        case KFNum(f) =>  str += "double "+f+ ", "
        case KChConst(c) => str += "i32 "+c + ", "
        case _ => str
      }
    }
    typ_val(KVar(x1),tyenv) match {
      case "Int" =>{
        s"call i32 @$x1 (${str.dropRight(2)})"
      }
      case "Double" =>{
        s"call double @$x1 (${str.dropRight(2)})"
      }
      case "Void" =>{
        s"call void @$x1 (${str.dropRight(2)})"
      }
    }
  }

  case KWrite(x1) =>
    s"call i32 @printInt (i32 ${compile_val(x1)})"
}

// compile K expressions
def compile_exp(a: KExp) : String = a match {
  case KReturn(v) =>{
    typ_val(v,tyenv) match {
      case "Int" =>{
        i"ret i32 ${compile_val(v)}"
      }
      case "Double" =>{
        i"ret double ${compile_val(v)}"
      }
      case "Void" =>{
        i"ret void"
      }
    }
  }

  case KLet(x: String, v: KVal, e: KExp) => {
    typ_val(v,tyenv) match {
      case "Void" =>
        i"${compile_val(v)}" ++ compile_exp(e)
      case _ =>
        i"%$x = ${compile_val(v)}" ++ compile_exp(e)
    }
  }

  case KIf(x, e1, e2) => {
    val if_br = Fresh("if_branch")
    val else_br = Fresh("else_branch")
    i"br i1 %$x, label %$if_br, label %$else_br" ++
    l"\n$if_br" ++
    compile_exp(e1) ++
    l"\n$else_br" ++ 
    compile_exp(e2)
  }
}


val prelude = """
declare i32 @printf(i8*, ...)

@.str_nl = private constant [2 x i8] c"\0A\00"
@.str_star = private constant [2 x i8] c"*\00"
@.str_space = private constant [2 x i8] c" \00"

define void @new_line() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_nl, i32 0, i32 0
  %1 = call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_star() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_star, i32 0, i32 0
  %1 = call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_space() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_space, i32 0, i32 0
  %1 = call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @skip() #0 {
  ret void
}

@.str = private constant [3 x i8] c"%d\00"
@.str_char = private constant [3 x i8] c"%c\00"

define void @print_int(i32 %x) {
   %t0 = getelementptr [3 x i8], [3 x i8]* @.str, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
   ret void
}

define void @print_char(i32 %x) {
   %t0 = getelementptr [3 x i8], [3 x i8]* @.str_char, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
   ret void
}

; END OF BUILD-IN FUNCTIONS (prelude)

"""


// compile function for declarations and main
def compile_decl(d: Decl) : String = d match {
  case Def(name, args, ty,body) => ty match {
    case "Int" =>{
      var str = ""
      for(x <- args){
        x._2 match{
          case "Int" => str += "i32 %"+ x._1+", "
          case "Double" =>  str += "double %"+ x._1+", "
          case _ =>
        }
        tyenv += (x._1 -> x._2 )
      }
      tyenv += (name -> ty)
      m"define i32 @$name (${str.dropRight(2)}) {" ++
        compile_exp(CPSi(body)) ++
        m"}\n"
    }
    case "Double" =>{
      var str = ""
      for(x <- args){
        x._2 match{
          case "Int" => str += "i32 %"+ x._1+", "
          case "Double" =>  str += "double %"+ x._1+", "
          case _ =>
        }
        tyenv += (x._1 -> x._2 )
      }
      tyenv += (name -> ty)
      m"define double @$name (${str.dropRight(2)}) {" ++
        compile_exp(CPSi(body)) ++
        m"}\n"
    }
    case "Void" =>{
      var str = ""
      for(x <- args){
        x._2 match{
          case "Int" => str += "i32 %"+ x._1+", "
          case "Double" =>  str += "double %"+ x._1+", "
          case _ =>
        }
        tyenv += (x._1 -> x._2 )
      }
      tyenv += (name -> ty)
      m"define void @$name (${str.dropRight(2)}) {" ++
        compile_exp(CPSi(body)) ++
        m"}\n"
    }
  }
  case Main(body) => {
    m"define i32 @main() {" ++
    compile_exp(CPS(body)(_ => KReturn(KNum(0)))) ++
    m"}\n"
  }
  case Const(name,v) =>{
    tyenv += (name -> "Int")
    s"@$name = global i32 $v\n\n"
  }
  case FConst(name,x) =>{
    tyenv += (name -> "Double")
    s"@$name = global double $x\n\n"
  }
}


// main compiler functions
def compile(prog: List[Decl]) : String = 
  prelude ++ (prog.map(compile_decl).mkString)


@main
def main(fname: String) = {
    val path = os.pwd / fname
    val file = fname.stripSuffix("." ++ path.ext)
    val tks = tokenise(os.read(path))
    println("parsing.... it may take some mins")
    val ast = parse_tks(tks)
    println(compile(ast))
}

@main
def write(fname: String) = {
    val path = os.pwd / fname
    val file = fname.stripSuffix("." ++ path.ext)
    val tks = tokenise(os.read(path))
    println("parsing.... it may take some mins")
    val ast = parse_tks(tks)
    val code = compile(ast)
    os.write.over(os.pwd / (file ++ ".ll"), code)
}


@main
def test() = {
//  println("path")
//  val path = os.pwd / fname
//  println("file")
//  val file = fname.stripSuffix("." ++ path.ext)
//  println("tks")
//  val tks = tokenise(os.read(path))
  println("parses")
  val ast = List(FConst("Ymin",-1.3), FConst("Ymax",1.3), FConst("Ystep",0.05), FConst("Xmin",-2.1), FConst("Xmax",1.1), FConst("Xstep",0.02), Const("Maxiters",1000), Def("m_iter",List(("m","Int"), ("x","Double"), ("y","Double"), ("zr","Double"), ("zi","Double")),"Void",If(Bop("<=",Var("Maxiters"),Var("m")),Call("print_char",List(ChConst(32))),If(Bop("<=",FNum(4.0),Aop("+",Aop("*",Var("zi"),Var("zi")),Aop("*",Var("zr"),Var("zr")))),Call("print_char",List(Aop("+",ChConst(48),Aop("%",Var("m"),Num(10))))),Call("m_iter",List(Aop("+",Var("m"),Num(1)), Var("x"), Var("y"), Aop("+",Var("x"),Aop("-",Aop("*",Var("zr"),Var("zr")),Aop("*",Var("zi"),Var("zi")))), Aop("+",Aop("*",FNum(2.0),Aop("*",Var("zr"),Var("zi"))),Var("y"))))))), Def("x_iter",List(("x","Double"), ("y","Double")),"Void",If(Bop("<=",Var("x"),Var("Xmax")),Sequence(Call("m_iter",List(Num(0), Var("x"), Var("y"), FNum(0.0), FNum(0.0))),Call("x_iter",List(Aop("+",Var("x"),Var("Xstep")), Var("y")))),Call("skip",List()))), Def("y_iter",List(("y","Double")),"Void",If(Bop("<=",Var("y"),Var("Ymax")),Sequence(Call("x_iter",List(Var("Xmin"), Var("y"))),Sequence(Call("print_char",List(ChConst(10))),Call("y_iter",List(Aop("+",Var("y"),Var("Ystep")))))),Call("skip",List()))), Main(Call("y_iter",List(Var("Ymin")))))
  println("comple")
  val code = compile(ast)
  println("last")
  println(code)
//  os.write.over(os.pwd / (file ++ ".ll"), code)
}


